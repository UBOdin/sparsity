package sparsity.statement

import sparsity.{Expression, Column, Name}

sealed abstract class ColumnAnnotation

case class ColumnIsPrimaryKey() extends ColumnAnnotation
case class ColumnIsNullable() extends ColumnAnnotation
case class ColumnDefaultValue(v: Expression) extends ColumnAnnotation
case class ColumnIsForeignKey(to: Column) extends ColumnAnnotation

case class ColumnDefinition(name: Name, t: String, annotations: Seq[ColumnAnnotation])
