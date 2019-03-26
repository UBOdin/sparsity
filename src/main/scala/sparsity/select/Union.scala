package sparsity.select

object Union extends Enumeration {
  type Type = Value
  val All, Distinct = Value

  def toString(t: Type) = t match {
    case All => "UNION ALL"
    case Distinct => "UNION DISTINCT"
  }
}