package sparsity

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import sparsity.common.Name
import sparsity.common.expression._
import sparsity.common.parser.SQL
import sparsity.common.select._
import sparsity.common.statement._
import fastparse.Parsed

object StatementRoundTripSpec extends Properties("Statement round-trip") {

  val genName: Gen[Name] = for {
    first <- Gen.alphaChar
    rest <- Gen.listOfN(5, Gen.alphaNumChar)
  } yield Name((first +: rest).mkString)

  val genLongPrimitive: Gen[LongPrimitive] =
    Gen.chooseNum(0L, 10000L).map(LongPrimitive(_))

  val genDoublePrimitive: Gen[DoublePrimitive] =
    Gen.chooseNum(0.0, 10000.0).map(DoublePrimitive(_))

  val genStringPrimitive: Gen[StringPrimitive] =
    Gen.alphaNumStr.map(s => StringPrimitive(s.take(20)))

  val genBooleanPrimitive: Gen[BooleanPrimitive] =
    Gen.oneOf(true, false).map(BooleanPrimitive(_))

  val genPrimitive: Gen[PrimitiveValue] =
    Gen.oneOf(genLongPrimitive, genDoublePrimitive, genStringPrimitive, genBooleanPrimitive)

  val genColumn: Gen[Column] = for {
    col <- genName
    table <- Gen.option(genName)
  } yield Column(col, table)

  def genArithmetic(depth: Int): Gen[Arithmetic] = for {
    lhs <- genExpression(depth - 1)
    op <- Gen.oneOf(Arithmetic.Add, Arithmetic.Sub, Arithmetic.Mult, Arithmetic.Div)
    rhs <- genExpression(depth - 1)
  } yield Arithmetic(lhs, op, rhs)

  def genComparison(depth: Int): Gen[Comparison] = for {
    lhs <- genExpression(depth - 1)
    op <- Gen.oneOf(Comparison.Eq, Comparison.Neq, Comparison.Gt, Comparison.Lt, Comparison.Gte, Comparison.Lte)
    rhs <- genExpression(depth - 1)
  } yield Comparison(lhs, op, rhs)

  val genFunction: Gen[Function] = for {
    name <- genName
    numArgs <- Gen.chooseNum(0, 3)
    args <- Gen.listOfN(numArgs, genPrimitive.map(_.asInstanceOf[Expression]))
  } yield Function(name, Some(args))

  def genExpression(depth: Int): Gen[Expression] =
    if (depth <= 0)
      Gen.oneOf(genPrimitive, genColumn)
    else
      Gen.frequency(
        (3, genPrimitive),
        (2, genColumn),
        (1, genArithmetic(depth)),
        (1, genComparison(depth)),
        (1, genFunction)
      )

  implicit val arbExpression: Arbitrary[Expression] = Arbitrary(genExpression(2))

  val genSelectAll: Gen[SelectAll] = Gen.const(SelectAll())

  val genSelectExpression: Gen[SelectExpression] = for {
    expr <- genExpression(1)
    alias <- Gen.option(genName)
  } yield SelectExpression(expr, alias)

  val genSelectTarget: Gen[SelectTarget] =
    Gen.frequency((1, genSelectAll), (3, genSelectExpression))

  val genFromTable: Gen[FromTable] = for {
    table <- genName
    alias <- Gen.option(genName)
  } yield FromTable(None, table, alias)

  val genOrderBy: Gen[OrderBy] = for {
    expr <- genColumn.map(_.asInstanceOf[Expression])
    asc <- Gen.oneOf(true, false)
  } yield OrderBy(expr, asc)

  val genSelectBody: Gen[SelectBody] = for {
    distinct <- Gen.oneOf(true, false)
    numTargets <- Gen.chooseNum(1, 3)
    targets <- Gen.listOfN(numTargets, genSelectTarget)
    numFroms <- Gen.chooseNum(0, 2)
    froms <- Gen.listOfN(numFroms, genFromTable)
    where <- Gen.option(genExpression(1))
    numOrderBy <- Gen.chooseNum(0, 2)
    orderBys <- Gen.listOfN(numOrderBy, genOrderBy)
    limit <- Gen.option(Gen.chooseNum(1L, 100L))
    offset <- Gen.option(Gen.chooseNum(0L, 50L))
  } yield SelectBody(
    distinct = distinct,
    target = targets,
    from = froms,
    where = where,
    groupBy = None,
    having = None,
    orderBy = orderBys,
    limit = limit,
    offset = offset,
    union = None
  )

  val genSelect: Gen[Select] = genSelectBody.map(body => Select(body))

  val genUpdate: Gen[Update] = for {
    table <- genName
    numSets <- Gen.chooseNum(1, 3)
    sets <- Gen.listOfN(
      numSets,
      for {
        col <- genName
        expr <- genExpression(1)
      } yield (col, expr)
    )
    where <- Gen.option(genExpression(1))
  } yield Update(table, sets, where)

  val genDelete: Gen[Delete] = for {
    table <- genName
    where <- Gen.option(genExpression(1))
  } yield Delete(table, where)

  val genExplicitInsert: Gen[ExplicitInsert] = for {
    numRows <- Gen.chooseNum(1, 3)
    numCols <- Gen.chooseNum(1, 3)
    rows <- Gen.listOfN(numRows, Gen.listOfN(numCols, genPrimitive.map(_.asInstanceOf[Expression])))
  } yield ExplicitInsert(rows)

  val genInsert: Gen[Insert] = for {
    table <- genName
    numCols <- Gen.chooseNum(1, 3)
    cols <- Gen.listOfN(numCols, genName)
    values <- genExplicitInsert
  } yield Insert(table, Some(cols), values, orReplace = false)

  val genColumnAnnotation: Gen[ColumnAnnotation] =
    Gen.oneOf(Gen.const(ColumnIsPrimaryKey()), Gen.const(ColumnIsNotNullable()))

  val genColumnDefinition: Gen[ColumnDefinition] = for {
    name <- genName
    typeName <- Gen.oneOf("int", "float", "varchar").map(Name(_))
    typeArgs <-
      if (typeName.name == "varchar")
        Gen.const(Seq(LongPrimitive(255)))
      else
        Gen.const(Seq.empty[PrimitiveValue])
    numAnnotations <- Gen.chooseNum(0, 2)
    annotations <- Gen.listOfN(numAnnotations, genColumnAnnotation)
  } yield ColumnDefinition(name, typeName, typeArgs, annotations.distinct)

  val genCreateTable: Gen[CreateTable] = for {
    name <- genName
    numCols <- Gen.chooseNum(1, 3)
    cols <- Gen.listOfN(numCols, genColumnDefinition)
  } yield CreateTable(name, orReplace = false, cols, Seq.empty)

  val genCreateTableAs: Gen[CreateTableAs] = for {
    name <- genName
    query <- genSelectBody
  } yield CreateTableAs(name, orReplace = false, query)

  val genCreateView: Gen[CreateView] = for {
    name <- genName
    query <- genSelectBody
  } yield CreateView(name, orReplace = false, query, materialized = false, temporary = false)

  val genDropTable: Gen[DropTable] = for {
    name <- genName
    ifExists <- Gen.oneOf(true, false)
  } yield DropTable(name, ifExists)

  val genDropView: Gen[DropView] = for {
    name <- genName
    ifExists <- Gen.oneOf(true, false)
  } yield DropView(name, ifExists)

  val genExplain: Gen[Explain] = genSelectBody.map(Explain(_))

  val genStatement: Gen[Statement] = Gen.oneOf(
    genSelect,
    genUpdate,
    genDelete,
    genInsert,
    genCreateTable,
    genCreateTableAs,
    genCreateView,
    genDropTable,
    genDropView,
    genExplain
  )

  implicit val arbStatement: Arbitrary[Statement] = Arbitrary(genStatement)

  property("serialization and parsing round-trip") = Prop.forAll(genStatement) { stmt =>
    val sql = stmt.toSql
    SQL(sql) match {
      case Parsed.Success(parsed, _) =>
        val equal = parsed == stmt
        if (!equal) {
          println(s"Original: $stmt")
          println(s"SQL: $sql")
          println(s"Parsed: $parsed")
        }
        equal
      case f: Parsed.Failure =>
        println(s"Failed to parse: $sql")
        println(s"Error: ${f.trace().longMsg}")
        false
    }
  }
}
