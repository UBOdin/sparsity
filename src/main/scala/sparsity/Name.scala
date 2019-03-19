package sparsity

case class Name(name: String, quoted: Boolean = false)
{
  override def equals(other: Any): Boolean =
  {
    other match {
      case s: String => equals(s)
      case n: Name => equals(n)
      case _ => false
    }
  }
  def equals(other: Name): Boolean =
  {
    if(quoted || other.quoted){ name.equals(other.name) }
    else { name.equalsIgnoreCase(other.name) }
  }
  def equals(other: String): Boolean =
  {
    if(quoted){ name.equals(other) }
    else { name.equalsIgnoreCase(other) }
  }
  override def toString = if(quoted){ "`"+name+"`" } else { name }
  def +(other: Name)   = Name(name+other.name, quoted || other.quoted)
  def +(other: String) = Name(name+other, quoted)

  def lower = if(quoted){ name } else { name.toLowerCase }
  def upper = if(quoted){ name } else { name.toUpperCase }
}

class StringNameMatch(cmp:String)
{
  def unapply(name: Name):Option[Unit] = 
    if(name.equals(cmp)) { return Some(()) } 
    else { return None }
}

object NameMatch
{
  def apply(cmp: String) = new StringNameMatch(cmp)
}