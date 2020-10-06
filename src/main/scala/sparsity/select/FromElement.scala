package sparsity.select

import sparsity.Name
import sparsity.expression.{Expression, BooleanPrimitive}
import sparsity.json.JsonTable

sealed abstract class FromElement
{
  def aliases: Seq[Name]
  def withAlias(newAlias: Name): FromElement
  def toStringWithParensIfNeeded: String = toString
}

case class FromTable(schema: Option[Name], table: Name, alias: Option[Name]) extends FromElement
{
  def aliases = Seq(alias.getOrElse(table))
  override def toString = 
    schema.map { _.toString+"."}.getOrElse("") +
    table.toString + 
    alias.map { " AS "+_.toString }.getOrElse("")
  def withAlias(newAlias: Name) = FromTable(schema, table, Some(newAlias))

}
case class FromSelect(body: SelectBody, val alias: Name) extends FromElement
{
  def aliases = Seq(alias)
  override def toString =
    "(" + body.toString + ") AS "+alias
  def withAlias(newAlias: Name) = FromSelect(body, newAlias)
}
case class FromJoin(
  lhs: FromElement, 
  rhs: FromElement, 
  t: Join.Type = Join.Inner, 
  on: Expression = BooleanPrimitive(true), 
  alias: Option[Name] = None) extends FromElement
{ 
  def aliases = 
    alias.map { Seq(_) }.getOrElse(lhs.aliases ++ rhs.aliases)
  override def toStringWithParensIfNeeded: String = "(" + toString + ")"
  override def toString = {
    val baseString = (
      lhs.toStringWithParensIfNeeded + " " + 
      Join.toString(t) + " " +
      rhs.toStringWithParensIfNeeded + 
      (on match { case BooleanPrimitive(true)  => ""; case _ => " ON " + on.toString })
    )
    alias match {
      case Some(a) => "(" + baseString + ") AS "+a.toString
      case None => baseString
    }
  }
  def withAlias(newAlias: Name) = FromJoin(lhs, rhs, t, on, Some(newAlias))

}

object Join extends Enumeration
{
  type Type = Value
  val  Inner, Natural, LeftOuter, RightOuter, FullOuter = Value

  def toString(t: Type) = t match {
    case Inner => "JOIN"
    case Natural => "NATURAL JOIN"
    case LeftOuter => "LEFT OUTER JOIN"
    case RightOuter => "RIGHT OUTER JOIN"
    case FullOuter => "FULL OUTER JOIN"
  }
}

case class FromJson(
  alias: Name, 
  spec: JsonTable
)
  extends FromElement
{
  def aliases = Seq(alias)
  def withAlias(newAlias: Name) = FromJson(newAlias, spec)
}