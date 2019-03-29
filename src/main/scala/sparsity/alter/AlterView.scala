package sparsity.alter

sealed abstract class AlterViewAction

case class Materialize(add: Boolean) extends AlterViewAction
{ 
  override def toString = (if(add){ "" } else { "DROP " })+"MATERIALIZE"
}