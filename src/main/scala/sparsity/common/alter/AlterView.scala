package sparsity.common.alter

import sparsity.common.expression.ToSql

sealed abstract class AlterViewAction extends ToSql

case class Materialize(add: Boolean) extends AlterViewAction {
  override def toSql = (if (add) { "" }
                        else { "DROP " }) + "MATERIALIZE"
}
