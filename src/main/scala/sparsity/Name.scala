package sparsity

case class Name(name: String, quoted: Boolean = false)
{
  def equals(other: Name)
  {
    if(quoted || other.quoted){ name.equals(other.name) }
    else { name.equalsIgnoreCase(other.name) }
  }
  def equals(other: String)
  {
    if(quoted){ name.equals(other) }
    else { name.equalsIgnoreCase(other) }
  }
}