package sparsity.select

import sparsity.Name
import sparsity.expression.{Expression, BooleanPrimitive}

sealed abstract class FromElement
{
  def aliases: Seq[Name]
}

case class FromTable(schema: Option[Name], table: Name, alias: Option[Name]) extends FromElement
{
  def aliases = Seq(alias.getOrElse(table))
}
case class FromSelect(body: SelectBody, val alias: Name) extends FromElement
{
  def aliases = Seq(alias)
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
}

object Join extends Enumeration
{
  type Type = Value
  val  Inner, Natural, LeftOuter, RightOuter, FullOuter = Value
}