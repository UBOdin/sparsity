package sparsity.select

import sparsity.Name
import sparsity.expression.Expression

sealed abstract class SelectTarget

case class SelectAll() extends SelectTarget
case class SelectTable(table: Name) extends SelectTarget
case class SelectExpression(expression: Expression, alias: Option[Name] = None) extends SelectTarget
