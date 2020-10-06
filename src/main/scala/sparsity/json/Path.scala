package sparsity.json

import sparsity.Name

/**
 * A json path expression
 */
case class Path(elements: Seq[PathElement])
{
  override def toString = "$"+elements.mkString
  def isSingleton = elements.forall { _.isSingleton }
  def ++(other: Path) = Path(elements ++ other.elements)
}

/**
 * One step in the json path
 */
sealed trait PathElement
  { val isSingleton: Boolean }

/**
 * A json path step for json objects, specifically
 */
sealed trait ObjectElement extends PathElement

/**
 * A single child of an object
 */
case class ObjectSubscript(subscript: Name) extends ObjectElement
  { override def toString = "."+subscript
    val isSingleton = true }

/**
 * All children of the specified object
 */
case class ObjectWildcard() extends ObjectElement
  { override def toString = ".*"
    val isSingleton = false }

/**
 * A json path step for json arrays, specifically
 */
sealed trait ArrayElement extends PathElement

/**
 * A json path step referencing a contiguous block of array elements
 */
sealed trait ArrayElementLeaf extends ArrayElement
{ 
  def unbracketedString: String
  override def toString(): String = s"[${unbracketedString}]"
} 

/**
 * A single element of an array
 */
case class ArrayIndex(index: Long) extends ArrayElementLeaf
  { def unbracketedString = index.toString 
    val isSingleton = true }
/**
 * A contiguous range of array elements
 */
case class ArrayRange(start: Long, end: Long) extends ArrayElementLeaf
  { def unbracketedString = s"$start TO $end"
    val isSingleton = false }
/**
 * A sequence of one or more contiguous blocks of array elements
 */
case class ArrayBlocks(blocks: Seq[ArrayElementLeaf]) extends ArrayElement
  { override def toString = s"[${blocks.map { _.unbracketedString }.mkString(",")}]"
    val isSingleton = blocks match { 
      case Seq(x) => x.isSingleton
      case _ => false
    } }
/**
 * All array elements
 */
case class ArrayWildcard() extends ArrayElement
  { override def toString = "[*]"
    val isSingleton = false }
