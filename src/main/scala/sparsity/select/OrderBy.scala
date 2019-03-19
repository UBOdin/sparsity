package sparsity.select

import sparsity.expression.Expression

case class OrderBy(expression: Expression, ascending: Boolean) {
  def descending = !ascending
}