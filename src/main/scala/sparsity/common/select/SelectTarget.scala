package sparsity.common.select

import sparsity.common.Name
import sparsity.common.expression.{Expression, ToSql}

sealed abstract class SelectTarget extends ToSql

case class SelectAll() extends SelectTarget {
  override def toSql = "*"
}
case class SelectTable(table: Name) extends SelectTarget {
  override def toSql = table.toSql + ".*"
}
case class SelectExpression(expression: Expression, alias: Option[Name] = None) extends SelectTarget {
  override def toSql =
    expression.toSql ++ alias.map(" AS " + _.toSql).getOrElse("")
}
