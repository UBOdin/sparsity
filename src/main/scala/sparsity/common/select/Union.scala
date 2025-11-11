package sparsity.common.select

object Union extends Enumeration {
  type Type = Value
  val All, Distinct = Value

  def toSql(t: Type) = t match {
    case All => "UNION ALL"
    case Distinct => "UNION DISTINCT"
  }
}
