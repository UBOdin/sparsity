package sparsity.parser

import fastparse._, MultiLineWhitespace._
import scala.io._
import java.io._
import sparsity.Name
import sparsity.statement._
import sparsity.select._
import sparsity.expression.{Expression => Expr}
import sparsity.parser.{Expression => ExprParser}

object SQL
{
  def apply(input: String) = parse(input, statement(_))
  def apply(input: Reader) = 
    new StreamParser[Statement](
      parse(_:Iterator[String], statement(_)), 
      input
    )

  def statement[_:P]: P[Statement] = 
    P( 
      Pass()~ // This trims off leading whitespace
      ( select.map { Select(_) }
      | update
      | delete
      | insert
      | (&(StringInIgnoreCase("CREATE")) ~/ (
            createTable 
          | createView
        ))
      | dropTableOrView
      | explain
      ) ~ ";"
    )

  def explain[_:P] = P(
    StringInIgnoreCase("EXPLAIN") ~ select.map { Explain(_) }
  )

  def ifExists[_:P]:P[Boolean] = P(
    (StringInIgnoreCase("IF") ~
      StringInIgnoreCase("EXISTS")
    ).!.?.map { _ != None }
  )

  def dropTableOrView[_:P] = P(
    (
      StringInIgnoreCase("DROP") ~
      StringInIgnoreCase("TABLE", "VIEW").!.map { _.toUpperCase } ~/
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
    (StringInIgnoreCase("OR") ~
      StringInIgnoreCase("REPLACE")
    ).!.?.map { _ != None }
  )

  def createView[_:P] = P(
    (
      StringInIgnoreCase("CREATE") ~
      orReplace ~
      StringInIgnoreCase("VIEW") ~/
      Elements.identifier ~
      StringInIgnoreCase("AS") ~/
      select
    ).map { case (orReplace, name, query) => 
              CreateView(name, orReplace, query) }
  )

  def columnAnnotation[_:P]:P[ColumnAnnotation] = P(
    (
      StringInIgnoreCase("PRIMARY") ~/
        StringInIgnoreCase("KEY").map { _ => ColumnIsPrimaryKey() }
    ) | (
      StringInIgnoreCase("NOT") ~/
        StringInIgnoreCase("NULL").map { _ => ColumnIsNotNullable() }
    ) | (
      StringInIgnoreCase("DEFAULT") ~/
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
      StringInIgnoreCase("PRIMARY") ~/
        StringInIgnoreCase("KEY") ~ 
          oneOrMoreAttributes.map { attrs => Left(TablePrimaryKey(attrs)) }
    ) | (
      StringInIgnoreCase("INDEX") ~/
        StringInIgnoreCase("ON") ~ 
          oneOrMoreAttributes.map { attrs => Left(TableIndexOn(attrs)) }
    ) | (
      (
        Elements.identifier ~/ 
          Elements.identifier ~ 
            columnAnnotation.rep
      ).map { case (name, t, annotations) =>
                 Right(ColumnDefinition(name, t, annotations)) }
    )

  )

  def createTable[_:P] = P(
    (
      StringInIgnoreCase("CREATE") ~
      orReplace ~
      StringInIgnoreCase("TABLE") ~/
      Elements.identifier ~ "(" ~
        tableField.rep(sep = Elements.comma) ~
      ")"
    ).map { case (orReplace, table, fields) =>
      val columns = fields.collect { case Right(r) => r }
      val annotations = fields.collect { case Left(l) => l }
      CreateTable(table, orReplace, columns, annotations)
    }
  )

  def valueList[_:P]: P[InsertValues] = P(
    (
      StringInIgnoreCase("VALUES") ~/
      ("(" ~/ ExprParser.expressionList ~ ")").rep(sep = Elements.comma)
    ).map { ExplicitInsert(_) }
  )

  def insert[_:P] = P(
    (
      StringInIgnoreCase("INSERT") ~/
      (
        StringInIgnoreCase("OR") ~/
        StringInIgnoreCase("REPLACE")
      ).!.?.map { case None => false; case _ => true } ~
      StringInIgnoreCase("INTO") ~/
      Elements.identifier ~ 
      (("(" ~/
        Elements.identifier ~
        (Elements.comma ~/ Elements.identifier).rep ~
      ")").map { x => Seq(x._1)++x._2 }).? ~ 
      (
          (&(StringInIgnoreCase("SELECT")) ~/ select.map { SelectInsert(_) })
        | (&(StringInIgnoreCase("VALUES")) ~/ valueList)
      )
    ).map { case (orReplace, table, columns, values) => 
      Insert(table, columns, values, orReplace)
    }
  )

  def delete[_:P] = P(
    (
      StringInIgnoreCase("DELETE") ~/
      StringInIgnoreCase("FROM") ~/
      Elements.identifier ~
      (
        StringInIgnoreCase("WHERE") ~/
        ExprParser.expression
      ).?
    ).map { case (table, where) => Delete(table, where) }
  )

  def update[_:P] = P(
    (
      StringInIgnoreCase("UPDATE") ~/ 
      Elements.identifier ~ 
      StringInIgnoreCase("SET") ~/
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
    StringInIgnoreCase("AS").? ~ Elements.identifier
  )

  def selectTarget[_:P]: P[SelectTarget] = P(
      P("*").map { _ => SelectAll() } 
    | Elements.dottedWildcard.map { SelectTable(_) }
    | ( ExprParser.expression ~ alias.?).map 
        { x => SelectExpression(x._1, x._2) }
  )

  def fromElement[_:P] = P(
      (("(" ~ select ~ ")" ~ alias).map { x => FromSelect(x._1, x._2) })
    | ((Elements.dottedPair ~ alias.?).map { 
        case (schema, table, alias) => FromTable(schema, table, alias)
      })
  )

  def fromClause[_:P] = P(
    StringInIgnoreCase("FROM") ~/ 
      fromElement.rep(sep = Elements.comma, min=1)
  )

  def whereClause[_:P] = P(
    StringInIgnoreCase("WHERE") ~/ ExprParser.expression
  )

  def groupByClause[_:P] = P(
    StringInIgnoreCase("GROUP") ~/
    StringInIgnoreCase("BY") ~/
    ExprParser.expressionList
  )

  def havingClause[_:P] = P(
    StringInIgnoreCase("HAVING") ~ ExprParser.expression
  )

  def options[A](default: A, options: Map[String, A]): (Option[String] => A) =
    _.map { _.toUpperCase }.map { options(_) }.getOrElse(default)

  def ascOrDesc[_:P] = P(
    StringInIgnoreCase("ASC", "DESC").!.?.map { 
      options(true, Map("ASC" -> true, "DESC"-> false))
    }
  )

  def orderBy[_:P] = P(
    ( ExprParser.expression ~ ascOrDesc ).map { x => OrderBy(x._1, x._2) }
  )

  def orderByClause[_:P] = P(
    StringInIgnoreCase("ORDER") ~/
      StringInIgnoreCase("BY") ~/
        orderBy.rep(sep = Elements.comma, min = 1)
  )

  def limitClause[_:P] = P(
    StringInIgnoreCase("LIMIT") ~/
    Elements.integer
  )

  def offsetClause[_:P] = P(
    StringInIgnoreCase("OFFSET") ~/
    Elements.integer
  )

  def allOrDistinct[_:P] = P(
    StringInIgnoreCase("ALL", "DISTINCT").!.?.map { 
      options(Union.Distinct, Map("ALL" -> Union.All, "DISTINCT"-> Union.Distinct))
    }
  )

  def unionClause[_:P] = P(
    (StringInIgnoreCase("UNION") ~/ allOrDistinct ~/ select)
  )

  def select[_:P]: P[SelectBody] = P( 
    (
      "SELECT" ~/ 
      StringInIgnoreCase("DISTINCT").!.?.map { _ != None } ~/
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