package sparsity.parser

import fastparse._, MultiLineWhitespace._
import scala.io._
import java.io._
import sparsity.Name
import sparsity.statement._
import sparsity.select._
import sparsity.alter._
import sparsity.expression.{Expression => Expr, BooleanPrimitive}
import sparsity.parser.{Expression => ExprParser}
import sparsity.parser.Elements.{keyword => Keyword}

object SQL
{
  def apply(input: String) = parse(input, terminatedStatement(_))
  def apply(input: Reader) = 
    new StreamParser[Statement](
      parse(_:Iterator[String], terminatedStatement(_), verboseFailures = true), 
      input
    )

  def terminatedStatement[_:P]: P[Statement] = 
    P( statement ~ ";" )

  def statement[_:P]: P[Statement] = 
    P( 
      Pass()~ // This trims off leading whitespace
      ( parenthesizedSelect.map { Select(_) }
      | update
      | delete
      | insert
      | (&(Keyword("CREATE")) ~/ (
            createTable 
          | createView
        ))
      | (&(Keyword("ALTER")) ~/ (
            alterView
        ))
      | dropTableOrView
      | explain
      )
    )

  def explain[_:P] = P(
    Keyword("EXPLAIN") ~ select.map { Explain(_) }
  )

  def ifExists[_:P]:P[Boolean] = P(
    (Keyword("IF") ~
      Keyword("EXISTS")
    ).!.?.map { _ != None }
  )

  def alterView[_:P] = P(
    (
      Keyword("ALTER") ~
      Keyword("VIEW") ~/
      Elements.identifier ~
      (
          (Keyword("MATERIALIZE").!.map { _ => Materialize(true) })
        | ( Keyword("DROP") ~
            Keyword("MATERIALIZE").!.map { _ => Materialize(false) })
      )
    ).map { case (name, op) => AlterView(name, op) }
  )

  def dropTableOrView[_:P] = P(
    (
      Keyword("DROP") ~
      Keyword("TABLE", "VIEW").!.map { _.toUpperCase } ~/
      ifExists ~
      Elements.identifier
    ).map { 
      case ("TABLE", ifExists, name) => 
        DropTable(name, ifExists)
      case ("VIEW", ifExists, name) => 
        DropView(name, ifExists)
      case (_, _, _) =>
        throw new Exception("Internal Error")
    }
  )

  def orReplace[_:P]:P[Boolean] = P(
    (Keyword("OR") ~
      Keyword("REPLACE")
    ).!.?.map { _ != None }
  )

  def createView[_:P] = P(
    (
      Keyword("CREATE") ~
      orReplace ~
      Keyword(
        "MATERIALIZED",
        "TEMPORARY"
      ).!.?.map { 
        _.map { _.toUpperCase } match {
          case Some("MATERIALIZED") => (true, false)
          case Some("TEMPORARY") => (false, true)
          case _ => (false, false)
        }} ~
      Keyword("VIEW") ~/
      Elements.identifier ~
      Keyword("AS") ~/
      select
    ).map { case (orReplace, (materialized, temporary), name, query) => 
              CreateView(name, orReplace, query, materialized, temporary) }
  )

  def columnAnnotation[_:P]:P[ColumnAnnotation] = P(
    (
      Keyword("PRIMARY") ~/
        Keyword("KEY").map { _ => ColumnIsPrimaryKey() }
    ) | (
      Keyword("NOT") ~/
        Keyword("NULL").map { _ => ColumnIsNotNullable() }
    ) | (
      Keyword("DEFAULT") ~/
        (
          ("(" ~ ExprParser.expression ~ ")")
          | ExprParser.primitive
        ).map { ColumnDefaultValue(_) }
    )
  )

  def oneOrMoreAttributes[_:P]:P[Seq[Name]] = P(
    ( "(" ~/ Elements.identifier.rep(sep = Elements.comma, min = 1) ~ ")")
    | Elements.identifier.map { Seq(_) }
  )

  def tableField[_:P]:P[Either[TableAnnotation,ColumnDefinition]] = P(
    (
      Keyword("PRIMARY") ~/
        Keyword("KEY") ~ 
          oneOrMoreAttributes.map { attrs => Left(TablePrimaryKey(attrs)) }
    ) | (
      Keyword("INDEX") ~/
        Keyword("ON") ~ 
          oneOrMoreAttributes.map { attrs => Left(TableIndexOn(attrs)) }
    ) | (
      (
        Elements.identifier ~/ 
          Elements.identifier ~ 
            ( "(" ~
              ExprParser.primitive.rep( sep = "," ) ~ 
              ")"
            ).?.map { _.getOrElse(Seq()) } ~
            columnAnnotation.rep
      ).map { case (name, t, args, annotations) =>
                 Right(ColumnDefinition(name, t, args, annotations)) }
    )

  )

  def createTable[_:P] = P(
    (
      Keyword("CREATE") ~
      orReplace ~
      Keyword("TABLE") ~/
      Elements.identifier ~
      ( 
        ( Keyword("AS") ~/ select ).map { Left(_) }
        | ( "(" ~/
            tableField.rep(sep = Elements.comma) ~
          ")"
        ).map { Right(_) }
      )
    ).map { 
        case (orReplace, table, Left(query)) => 
          CreateTableAs(table, orReplace, query)
        case (orReplace, table, Right(fields)) =>
          val columns = fields.collect { case Right(r) => r }
          val annotations = fields.collect { case Left(l) => l }
          CreateTable(table, orReplace, columns, annotations)
    }
  )

  def valueList[_:P]: P[InsertValues] = P(
    (
      Keyword("VALUES") ~/
      ("(" ~/ ExprParser.expressionList ~ ")").rep(sep = Elements.comma)
    ).map { ExplicitInsert(_) }
  )

  def insert[_:P] = P(
    (
      Keyword("INSERT") ~/
      (
        Keyword("OR") ~/
        Keyword("REPLACE")
      ).!.?.map { case None => false; case _ => true } ~
      Keyword("INTO") ~/
      Elements.identifier ~ 
      (("(" ~/
        Elements.identifier ~
        (Elements.comma ~/ Elements.identifier).rep ~
      ")").map { x => Seq(x._1)++x._2 }).? ~ 
      (
          (&(Keyword("SELECT")) ~/ select.map { SelectInsert(_) })
        | (&(Keyword("VALUES")) ~/ valueList)
      )
    ).map { case (orReplace, table, columns, values) => 
      Insert(table, columns, values, orReplace)
    }
  )

  def delete[_:P] = P(
    (
      Keyword("DELETE") ~/
      Keyword("FROM") ~/
      Elements.identifier ~
      (
        Keyword("WHERE") ~/
        ExprParser.expression
      ).?
    ).map { case (table, where) => Delete(table, where) }
  )

  def update[_:P] = P(
    (
      Keyword("UPDATE") ~/ 
      Elements.identifier ~ 
      Keyword("SET") ~/
      ( 
        Elements.identifier ~
        "=" ~/
        ExprParser.expression
      ).rep(sep = Elements.comma, min = 1) ~
      (
        StringInIgnoreCase("WHERE") ~/
        ExprParser.expression
      ).?
    ).map { 
      case (table, set, where) =>
        Update(
          table, 
          set, 
          where
        )
    }
  )

  def alias[_:P]: P[Name] = P(
    Keyword("AS").? ~ Elements.identifier
  )

  def selectTarget[_:P]: P[SelectTarget] = P(
      P("*").map { _ => SelectAll() } 
      // Dotted wildcard needs a lookahead since a single token isn't
      // enough to distinguish between `foo`.* and `foo` AS `bar`
    | ( &(Elements.dottedWildcard) ~ 
          Elements.dottedWildcard.map { SelectTable(_) } )
    | ( ExprParser.expression ~ alias.?).map 
        { x => SelectExpression(x._1, x._2) }
  )

  def simpleFromElement[_:P]: P[FromElement] = P(
      (("(" ~ select ~ ")" ~ alias).map { x => FromSelect(x._1, x._2) })
    | ((Elements.dottedPair ~ alias.?).map { 
        case (schema, table, alias) => FromTable(schema, table, alias)
      })
    | (("(" ~ fromElement ~ ")" ~ alias.?).map { 
        case (from, None)        => from
        case (from, Some(alias)) => from.withAlias(alias)
      })
  )

  def joinWith[_:P]: P[Join.Type] = P(
      Keyword("JOIN").map { Unit => Join.Inner } 
    | ( (
          Keyword("NATURAL").!.map { Unit => Join.Natural}
        | Keyword("INNER").map { Unit => Join.Inner }
        | ( ( 
                Keyword("LEFT").map { Unit => Join.LeftOuter }
              | Keyword("RIGHT").map { Unit => Join.RightOuter }
              | Keyword("FULL").map { Unit => Join.FullOuter }
            ).?.map { _.getOrElse(Join.FullOuter) } ~/
            Keyword("OUTER")
          )
        ) ~/ Keyword("JOIN")
      )
  )

  def fromElement[_:P]: P[FromElement] = P(
    (
      simpleFromElement ~ (
        &(joinWith) ~
        joinWith ~/
        simpleFromElement ~/
        (
          Keyword("ON") ~/
          ExprParser.expression
        ).? ~
        alias.?
      ).rep 
    ).map { case (lhs, rest) => 
      rest.foldLeft(lhs) { (lhs, next) =>
        val (t, rhs, onClause, alias) = next
        FromJoin(
          lhs, 
          rhs, 
          t, 
          onClause.getOrElse(BooleanPrimitive(true)), 
          alias
        )
      }
    }
  )

  def fromClause[_:P] = P(
    Keyword("FROM") ~/ 
      fromElement.rep(sep = Elements.comma, min=1)
  )

  def whereClause[_:P] = P(
    Keyword("WHERE") ~/ ExprParser.expression
  )

  def groupByClause[_:P] = P(
    Keyword("GROUP") ~/
    Keyword("BY") ~/
    ExprParser.expressionList
  )

  def havingClause[_:P] = P(
    Keyword("HAVING") ~ ExprParser.expression
  )

  def options[A](default: A, options: Map[String, A]): (Option[String] => A) =
    _.map { _.toUpperCase }.map { options(_) }.getOrElse(default)

  def ascOrDesc[_:P] = P(
    Keyword("ASC", "DESC").!.?.map { 
      options(true, Map("ASC" -> true, "DESC"-> false))
    }
  )

  def orderBy[_:P] = P(
    ( ExprParser.expression ~ ascOrDesc ).map { x => OrderBy(x._1, x._2) }
  )

  def orderByClause[_:P] = P(
    Keyword("ORDER") ~/
      Keyword("BY") ~/
        orderBy.rep(sep = Elements.comma, min = 1)
  )

  def limitClause[_:P] = P(
    Keyword("LIMIT") ~/
    Elements.integer
  )

  def offsetClause[_:P] = P(
    Keyword("OFFSET") ~/
    Elements.integer
  )

  def allOrDistinct[_:P] = P(
    Keyword("ALL", "DISTINCT").!.?.map { 
      options(Union.Distinct, Map("ALL" -> Union.All, "DISTINCT"-> Union.Distinct))
    }
  )

  def unionClause[_:P] = P(
    (Keyword("UNION") ~/ allOrDistinct ~/ parenthesizedSelect)
  )

  def parenthesizedSelect[_:P]: P[SelectBody] = P(
    (
      "(" ~/ select ~ ")" ~/ unionClause.?
    ).map { 
      case (body, Some((unionType, unionBody))) => body.unionWith(unionType, unionBody) 
      case (body, None) => body
    } | select
  )

  def select[_:P]: P[SelectBody] = P( 
    (
      Keyword("SELECT") ~/ 
      Keyword("DISTINCT").!.?.map { _ != None } ~/
      selectTarget.rep(sep = ",") ~
      fromClause.?.map { _.toSeq.flatten } ~
      whereClause.? ~
      groupByClause.? ~
      havingClause.? ~
      orderByClause.?.map { _.toSeq.flatten } ~
      limitClause.? ~
      offsetClause.? ~
      unionClause.?
    ).map { case (distinct, targets, froms, where, groupBy, having, orderBy, limit, offset, union) => 
      SelectBody(
        distinct = distinct,
        target = targets,
        from = froms,
        where = where,
        groupBy = groupBy,
        having = having,
        orderBy = orderBy,
        limit = limit,
        offset = offset,
        union = union
      )
    }
  )
}