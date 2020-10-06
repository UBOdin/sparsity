package sparsity.json

import sparsity.expression.{ Expression => SparsityExpression }

case class JsonTable(
  target: SparsityExpression, 
  path: Path, 
  columns: Seq[Column]
)
{
  override def toString =
    s"JSON_TABLE($target, $path) COLUMNS(${columns.mkString(",")})"
}