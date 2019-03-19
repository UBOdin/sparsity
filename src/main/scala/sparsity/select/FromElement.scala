package sparsity.select

import sparsity.Name
import sparsity.expression.Expression

sealed abstract class FromElement

case class FromTable(schema: Option[Name], table: Name, alias: Option[Name]) extends FromElement
case class FromSelect(body: SelectBody, alias: Name) extends FromElement
case class FromJoin(lhs: FromElement, rhs: FromElement, t: Join.Type, on: Expression) extends FromElement

object Join extends Enumeration
{
  type Type = Value
  val  Inner, Natural, LeftOuter, RightOuter, FullOuter = Value
}