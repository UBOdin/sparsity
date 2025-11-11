package sparsity.common.select

import sparsity.common.Name
import sparsity.common.expression.{BooleanPrimitive, Expression}

sealed abstract class FromElement {
  def aliases: Seq[Name]
  def withAlias(newAlias: Name): FromElement
  def toSqlWithParensIfNeeded: String = toSql
  def toSql: String
}

case class FromTable(schema: Option[Name], table: Name, alias: Option[Name]) extends FromElement {
  def aliases = Seq(alias.getOrElse(table))
  override def toSql =
    schema.map(_.toSql + ".").getOrElse("") +
      table.toSql +
      alias.map(" AS " + _.toSql).getOrElse("")
  def withAlias(newAlias: Name) = FromTable(schema, table, Some(newAlias))

}
case class FromSelect(body: SelectBody, val alias: Name) extends FromElement {
  def aliases = Seq(alias)
  override def toSql =
    "(" + body.toSql + ") AS " + alias.toSql
  def withAlias(newAlias: Name) = FromSelect(body, newAlias)
}
case class FromJoin(
  lhs: FromElement,
  rhs: FromElement,
  t: Join.Type = Join.Inner,
  on: Expression = BooleanPrimitive(true),
  alias: Option[Name] = None
) extends FromElement {
  def aliases =
    alias.map(Seq(_)).getOrElse(lhs.aliases ++ rhs.aliases)
  override def toSqlWithParensIfNeeded: String = "(" + toSql + ")"
  override def toSql = {
    val baseString =
      lhs.toSqlWithParensIfNeeded + " " +
        Join.toSql(t) + " " +
        rhs.toSqlWithParensIfNeeded +
        (on match { case BooleanPrimitive(true) => ""; case _ => " ON " + on.toSql })
    alias match {
      case Some(a) => "(" + baseString + ") AS " + a.toSql
      case None => baseString
    }
  }
  def withAlias(newAlias: Name) = FromJoin(lhs, rhs, t, on, Some(newAlias))

}

object Join extends Enumeration {
  type Type = Value
  val Inner, Natural, LeftOuter, RightOuter, FullOuter = Value

  def toSql(t: Type) = t match {
    case Inner => "JOIN"
    case Natural => "NATURAL JOIN"
    case LeftOuter => "LEFT OUTER JOIN"
    case RightOuter => "RIGHT OUTER JOIN"
    case FullOuter => "FULL OUTER JOIN"
  }
}
