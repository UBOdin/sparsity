package sparsity.common.statement

import sparsity.common.Name
import sparsity.common.expression.ToSql

sealed abstract class TableAnnotation extends ToSql

case class TablePrimaryKey(columns: Seq[Name]) extends TableAnnotation {
  override def toSql = s"PRIMARY KEY (${columns.map(_.toSql).mkString(", ")})"
}
case class TableIndexOn(columns: Seq[Name]) extends TableAnnotation {
  override def toSql = s"INDEX (${columns.map(_.toSql).mkString(", ")})"
}
case class TableUnique(columns: Seq[Name]) extends TableAnnotation {
  override def toSql = s"UNIQUE (${columns.map(_.toSql).mkString(", ")})"
}
