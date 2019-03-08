package sparsity.statement

import sparsity.Expression

case class OrderBy(expression: Expression, ascending: Boolean) {
  def descending = !ascending
}