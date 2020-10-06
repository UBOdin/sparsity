package sparsity.json

import sparsity.Name
import sparsity.expression.{ Expression => SparsityExpression }

sealed trait Expression
{
  def children: Seq[SparsityExpression]
  def rebuild(c: Seq[SparsityExpression]): Expression
}

sealed trait PrimitiveExpression extends Expression

case class JsonExists(
  target: SparsityExpression, 
  path: Path,
  dataType: Name = Name("varchar2")
) extends PrimitiveExpression
{
  override def toString =
    s"JSON_EXISTS($target, $path)"
  def children: Seq[SparsityExpression] = Seq()
  def rebuild(c: Seq[SparsityExpression]): Expression = this
}

case class JsonQuery(
  target: SparsityExpression, 
  path: Path,
  wrapper: QueryResultWrapper,
  dataType: Name = Name("varchar2")
) extends PrimitiveExpression
{
  override def toString =
    s"JSON_QUERY($target, $path"+
    (if(dataType.equals(Name("varchar2"))){""} else { "RETURNING" + dataType })+
    wrapper.toString+")"
  def children: Seq[SparsityExpression] = Seq()
  def rebuild(c: Seq[SparsityExpression]): Expression = this
}

case class JsonValue(
  target: SparsityExpression, 
  path: Path,
  dataType: Name = Name("varchar2")
) extends PrimitiveExpression
{
  override def toString =
    s"JSON_VALUE($target, $path)"
  def children: Seq[SparsityExpression] = Seq()
  def rebuild(c: Seq[SparsityExpression]): Expression = this
}

case class JsonOrdinal() extends PrimitiveExpression
{
  def children: Seq[SparsityExpression] = Seq()
  def rebuild(c: Seq[SparsityExpression]): Expression = this
}
