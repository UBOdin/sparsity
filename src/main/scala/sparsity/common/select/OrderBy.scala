package sparsity.common.select

import sparsity.common.expression.{Expression, ToSql}

case class OrderBy(expression: Expression, ascending: Boolean) extends ToSql {
  def descending = !ascending
  override def toSql = expression.toSql + (if (ascending) { "" }
                                           else { " DESC" })
}
