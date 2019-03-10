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
}