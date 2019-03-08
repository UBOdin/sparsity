package sparsity.statement

import sparsity.Name

sealed abstract class TableAnnotation

case class TablePrimaryKey(columns: Seq[Name]) extends TableAnnotation