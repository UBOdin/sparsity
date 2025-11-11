package sparsity.common

import sparsity.common.expression.ToSql

case class Name(name: String, quoted: scala.Boolean = false) extends ToSql {
  override def equals(other: Any): scala.Boolean =
    other match {
      case s: String => equals(s)
      case n: Name => equals(n)
      case _ => false
    }
  def equals(other: Name): scala.Boolean =
    if (quoted || other.quoted) { name.equals(other.name) }
    else { name.equalsIgnoreCase(other.name) }
  def equals(other: String): scala.Boolean =
    if (quoted) { name.equals(other) }
    else { name.equalsIgnoreCase(other) }
  override def toSql = if (quoted) { "`" + name + "`" }
  else { name }
  def +(other: Name) = Name(name + other.name, quoted || other.quoted)
  def +(other: String) = Name(name + other, quoted)

  def lower = if (quoted) { name }
  else { name.toLowerCase }
  def upper = if (quoted) { name }
  else { name.toUpperCase }

  def extendUpper(template: String) =
    Name(template.replace("\\$", upper), quoted)
  def extendLower(template: String) =
    Name(template.replace("\\$", lower), quoted)

  def withPrefix(other: String) = Name(other + name, quoted)
  def withSuffix(other: String) = Name(name + other, quoted)
}

class StringNameMatch(cmp: String) {
  def unapply(name: Name): Option[Unit] =
    if (name.equals(cmp): scala.Boolean) { return Some(()) }
    else { return None }
}

object NameMatch {
  def apply(cmp: String) = new StringNameMatch(cmp)
}
