package sparsity.statement

import sparsity.Expression

sealed abstract class InsertValues

case class ExplicitInsert(values: Seq[Seq[Expression]]) extends InsertValues
{
  override def toString() = 
    "VALUES "+values.map { "("+_.mkString(", ")+")" }.mkString(", ")
}
case class SelectInsert(query: Select) extends InsertValues
{
  override def toString() =
    "("+query+")"
}