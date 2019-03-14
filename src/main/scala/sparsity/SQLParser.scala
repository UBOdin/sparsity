package sparsity

import sparsity.statement._
import fastparse._, MultiLineWhitespace._
import scala.io._
import java.io._

object SQLParser
{
  def apply(input: String) = parse(input, statement(_))
  def apply(input: Reader) = 
    new StreamParser[Statement](
      parse(_:Iterator[String], statement(_)), 
      input
    )

  def alias[_:P]: P[Name] = P(
    StringInIgnoreCase("AS") ~ NameParser.identifier
  )
  def aliasWithOptionalAs[_:P]: P[Name] = P(
    StringInIgnoreCase("AS").? ~ NameParser.identifier
  )

  def selectTarget[_:P]: P[SelectTarget] = P(
      P("*").map { _ => SelectAll() } 
    | NameParser.dottedWildcard.map { SelectTable(_) }
    | ( ExpressionParser.expression ~ alias.?).map 
        { x => SelectExpression(x._1, x._2) }
  )

  def selectTargets[_:P] = P(
    (selectTarget ~ ("," ~ selectTarget).rep).map 
      { case (first, rest) => Seq(first) ++ rest }
  )

  def fromElement[_:P] = P(
      (("(" ~ select ~ ")" ~ aliasWithOptionalAs).map { x => FromSelect(x._1, x._2) })
    | ((NameParser.dottedPair ~ aliasWithOptionalAs.?).map { 
        case (schema, table, alias) => FromTable(schema, table, alias)
      })
  )

  def fromClause[_:P] = P(
    StringInIgnoreCase("FROM") ~/
      (fromElement ~ ("," ~ fromElement).rep).map 
        { case (first, rest) => Seq(first) ++ rest }
    
  )

  def whereClause[_:P] = P(
    StringInIgnoreCase("WHERE") ~/ ExpressionParser.expression
  )

  def groupByClause[_:P] = P(
    StringInIgnoreCase("GROUP") ~/
    StringInIgnoreCase("BY") ~/
    ExpressionParser.expressionList
  )

  def havingClause[_:P] = P(
    StringInIgnoreCase("HAVING") ~ ExpressionParser.expression
  )

  def options[A](default: A, options: Map[String, A]): (Option[String] => A) =
    _.map { _.toUpperCase }.map { options(_) }.getOrElse(default)

  def ascOrDesc[_:P] = P(
    StringInIgnoreCase("ASC", "DESC").!.?.map { 
      options(true, Map("ASC" -> true, "DESC"-> false))
    }
  )

  def orderBy[_:P] = P(
    ( ExpressionParser.expression ~ ascOrDesc ).map { x => OrderBy(x._1, x._2) }
  )

  def orderByClause[_:P] = P(
    StringInIgnoreCase("ORDER") ~/
    StringInIgnoreCase("BY") ~/
    (orderBy ~ ("," ~ orderBy).rep).map { case (first, rest) => Seq(first) ++ rest }
  )

  def limitClause[_:P] = P(
    StringInIgnoreCase("LIMIT") ~/
    ExpressionParser.integer
  )

  def offsetClause[_:P] = P(
    StringInIgnoreCase("OFFSET") ~/
    ExpressionParser.integer
  )

  def allOrDistinct[_:P] = P(
    StringInIgnoreCase("ALL", "DISTINCT").!.?.map { 
      options(Union.Distinct, Map("ALL" -> Union.All, "DISTINCT"-> Union.Distinct))
    }
  )

  def unionClause[_:P] = P(
    (StringInIgnoreCase("UNION") ~/ allOrDistinct ~/ select)
  )

  def select[_:P]: P[Select] = P( 
    (
      "SELECT" ~/ 
      StringInIgnoreCase("DISTINCT").!.?.map { _ != None } ~/
      selectTargets ~
      fromClause.?.map { _.toSeq.flatten } ~
      whereClause.? ~
      groupByClause.? ~
      havingClause.? ~
      orderByClause.?.map { _.toSeq.flatten } ~
      limitClause.? ~
      offsetClause.? ~
      unionClause.?
    ).map { case (distinct, targets, froms, where, groupBy, having, orderBy, limit, offset, union) => 
      Select(
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

  def statement[_:P]: P[Statement] = 
    P( 
      (
        select.map { SelectStatement(_) }
      ) ~ ";"
    )

}