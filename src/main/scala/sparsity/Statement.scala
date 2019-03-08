package sparsity

import sparsity.statement._

sealed abstract class Statement;

case class SelectStatement(
  body: Select, 
  withClause: Seq[WithClause]
) extends Statement

case class UpdateStatement(
  table: Name, 
  set: Seq[(Name, Expression)],
  where: Expression
) extends Statement

case class DeleteStatement(
  table: Name, 
  where: Expression
) extends Statement

case class InsertStatement(
  table: Name, 
  columns: Seq[Name], 
  values: Seq[InsertTarget]
) extends Statement

case class CreateTableStatement(
  table: Name,
  columns: Seq[ColumnDefinition],
  annotations: Seq[TableAnnotation]
) extends Statement