package sparsity.statement

import sparsity.Expression

case class Select(
  target: Seq[SelectTarget],
  from: Seq[FromElement],
  where: Option[Expression],
  groupBy: Seq[Expression],
  having: Option[Expression],
  orderBy: Seq[OrderBy],
  limit: Option[Int],
  offset: Option[Int],
  union: Option[(Select, Union.Type)]
)
