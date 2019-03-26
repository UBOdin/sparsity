package sparsity.select

import sparsity.Name
import sparsity.expression.Expression

sealed abstract class SelectTarget

case class SelectAll() extends SelectTarget
{
  override def toString = "*"
}
case class SelectTable(table: Name) extends SelectTarget
{
  override def toString = table.toString+".*"
}
case class SelectExpression(expression: Expression, alias: Option[Name] = None) extends SelectTarget
{
  override def toString = 
    expression.toString ++ alias.map { " AS "+_ }.getOrElse("")
}
