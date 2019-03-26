package sparsity.statement

import sparsity.expression.{Expression,PrimitiveValue}
import sparsity.Name

sealed abstract class ColumnAnnotation

case class ColumnIsPrimaryKey() extends ColumnAnnotation
{
  override def toString = "PRIMARY KEY"
}
case class ColumnIsNotNullable() extends ColumnAnnotation
{
  override def toString = "NOT NULL"
}
case class ColumnDefaultValue(v: Expression) extends ColumnAnnotation
{
  override def toString = "DEFAULT VALUE "+v.toString
}

case class ColumnDefinition(name: Name, t: Name, args: Seq[PrimitiveValue] = Seq(), annotations: Seq[ColumnAnnotation] = Seq())
{
  override def toString = (
    name.toString + " " + 
    t + (if(args.isEmpty) { "" } else { "(" + args.mkString(", ") + ")" } )+
    annotations.map { " "+_.toString }.mkString
  )
}
