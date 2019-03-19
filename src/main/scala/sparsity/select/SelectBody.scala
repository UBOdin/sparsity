package sparsity.select

import sparsity.expression.Expression

case class SelectBody(
  distinct: Boolean = false,
  target: Seq[SelectTarget] = Seq(),
  from: Seq[FromElement] = Seq(),
  where: Option[Expression] = None,
  groupBy: Option[Seq[Expression]] = None,
  having: Option[Expression] = None,
  orderBy: Seq[OrderBy] = Seq(),
  limit: Option[Long] = None,
  offset: Option[Long] = None,
  union: Option[(Union.Type, SelectBody)] = None
)
