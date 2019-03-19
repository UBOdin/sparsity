package sparsity.statement

import sparsity.expression.Expression
import sparsity.select.SelectBody

sealed abstract class InsertValues

case class ExplicitInsert(values: Seq[Seq[Expression]]) extends InsertValues
{
  override def toString() = 
    "VALUES "+values.map { "("+_.mkString(", ")+")" }.mkString(", ")
}
case class SelectInsert(query: SelectBody) extends InsertValues
{
  override def toString() =
    "("+query+")"
}