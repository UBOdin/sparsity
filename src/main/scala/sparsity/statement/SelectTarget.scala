package sparsity.statement

import sparsity.{Name, Expression}

sealed abstract class SelectTarget

case class SelectAll() extends SelectTarget
case class SelectTable(table: Name) extends SelectTarget
case class SelectExpression(expression: Expression, alias: Name)
