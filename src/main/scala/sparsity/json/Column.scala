package sparsity.json

import sparsity.Name
import sparsity.expression.{ Expression => SparsityExpression }

sealed trait Column
{
  def toExpressions(expr: SparsityExpression, base: Path):Seq[(Name,Expression)]
}
case class JsonExistsColumn(
  name: Name, 
  dataType: Name, 
  path: Path
) extends Column
{ 
  override def toString = s"$name $dataType EXISTS PATH $path" 
  def toExpressions(expr: SparsityExpression, base: Path) = 
    Seq(name -> JsonExists(expr, base ++ path, dataType))
}

case class JsonQueryColumn(
  name: Name, 
  dataType: Name, 
  path: Path,
  wrapper: QueryResultWrapper
) extends Column
{ 
  override def toString = s"$name $dataType FORMAT JSON $wrapper PATH $path" 
  def toExpressions(expr: SparsityExpression, base: Path) = 
    Seq(name -> JsonQuery(expr, base ++ path, wrapper, dataType))
}

case class JsonValueColumn(
  name: Name, 
  dataType: Name, 
  path: Path
) extends Column
{ 
  override def toString = s"$name $dataType PATH $path" 
  def toExpressions(expr: SparsityExpression, base: Path) = 
    Seq(name -> JsonValue(expr, base ++ path, dataType))
}

case class NestedColumns(path: Path, columns: Seq[Column]) extends Column
{
  override def toString =
    s"NESTED PATH $path COLUMNS(${columns.mkString(",")})"
  def toExpressions(expr: SparsityExpression, base: Path) = 
    columns.flatMap { _.toExpressions(expr, base ++ path) }
}
case class OrdinalityColumn(name: Name) extends Column
{
  override def toString =
    s"$name FOR ORDINALITY"
  def toExpressions(expr: SparsityExpression, base: Path) =
    Seq(name -> JsonOrdinal())
}