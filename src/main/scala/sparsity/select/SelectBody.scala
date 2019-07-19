package sparsity.select

import sparsity.expression.Expression

case class SelectBody(
  distinct: Boolean = false,
  target: Seq[SelectTarget] = Seq(),
  from: Seq[FromElement] = Seq(),
  where: Option[Expression] = None,
  groupBy: Option[Seq[Expression]] = None,
  having: Option[Expression] = None,
  orderBy: Seq[OrderBy] = Seq(),
  limit: Option[Long] = None,
  offset: Option[Long] = None,
  union: Option[(Union.Type, SelectBody)] = None
) {
  def stringElements: Seq[String] = 
    (
      Seq("SELECT") ++ 
      ( if(distinct) { Some("DISTINCT") } else { None } ) ++
      Seq(target.map { _.toString }.mkString(", "))++
      ( if(from.isEmpty) { None } else { Seq("FROM", from.map { _.toString }.mkString(", ")) } ) ++
      ( where.map { x => Seq("WHERE", x.toString) }.toSeq.flatten ) ++
      ( groupBy.map { x => Seq("GROUP BY") ++ x.map { _.toString } }.toSeq.flatten ) ++
      ( having.map { x => Seq("HAVING", x.toString) }.toSeq.flatten ) ++
      ( if(orderBy.isEmpty) { Seq() } else { Seq("ORDER BY", orderBy.map { _.toString }.mkString(", ")) } ) ++
      ( limit.map { x => Seq("LIMIT", x.toString) }.toSeq.flatten ) ++
      ( offset.map { x => Seq("OFFSET", x.toString) }.toSeq.flatten ) ++
      ( union.map { case (t, b) => Seq(Union.toString(t)) ++ b.stringElements }.toSeq.flatten )
    )
  override def toString = stringElements.mkString(" ")

  def unionWith(t: Union.Type, body: SelectBody): SelectBody = { 
    val replacementUnion = 
      union match { 
        case Some(nested) => (nested._1, nested._2.unionWith(t, body))
        case None => (t, body)
      }

    SelectBody(
      distinct,
      target,
      from,
      where,
      groupBy,
      having,
      orderBy,
      limit,
      offset,
      union = Some(replacementUnion)
    )
  }
}
