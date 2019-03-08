package sparsity.statement

import sparsity.PrimitiveValue

sealed abstract class InsertTarget

case class ExplicitInsert(values: Seq[Seq[PrimitiveValue]]) extends InsertTarget
case class SelectInsert(values: Select) extends InsertTarget