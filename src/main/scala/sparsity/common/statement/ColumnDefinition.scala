package sparsity.common.statement

import sparsity.common.expression.{Expression, PrimitiveValue, ToSql}
import sparsity.common.Name

sealed abstract class ColumnAnnotation extends ToSql

case class ColumnIsPrimaryKey() extends ColumnAnnotation {
  override def toSql = "PRIMARY KEY"
}
case class ColumnIsNotNullable() extends ColumnAnnotation {
  override def toSql = "NOT NULL"
}
case class ColumnIsNullable() extends ColumnAnnotation {
  override def toSql = "NULL"
}
case class ColumnDefaultValue(v: Expression) extends ColumnAnnotation {
  override def toSql = "DEFAULT VALUE " + v.toSql
}

case class ColumnDefinition(
  name: Name,
  t: Name,
  args: Seq[PrimitiveValue] = Seq(),
  annotations: Seq[ColumnAnnotation] = Seq()
) {
  def toSql =
    name.toSql + " " +
      t.toSql + (if (args.isEmpty) { "" }
                 else { "(" + args.map(_.toSql).mkString(", ") + ")" }) +
      annotations.map(" " + _.toSql).mkString
}
