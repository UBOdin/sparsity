package sparsity.oracle

import sparsity.common.Name
import sparsity.common.select.SelectBody
import sparsity.common.expression.Expression
import sparsity.common.alter.AlterViewAction
import sparsity.common.statement._
import sparsity.common.expression.ToSql

/** Oracle-specific statement types. Extends the base Statement ADT with Oracle-specific features.
  */
sealed trait OracleStatement extends Statement

/** Wrapper for common statements to make them OracleStatement-compatible.
  */
case class OracleInsert(table: Name, columns: Option[Seq[Name]], values: InsertValues, orReplace: Boolean)
    extends OracleStatement {
  override def toSql = Insert(table, columns, values, orReplace).toSql
}

case class OracleUpdate(table: Name, set: Seq[(Name, Expression)], where: Option[Expression]) extends OracleStatement {
  override def toSql = Update(table, set, where).toSql
}

case class OracleDelete(table: Name, where: Option[Expression]) extends OracleStatement {
  override def toSql = Delete(table, where).toSql
}

case class OracleSelect(body: SelectBody, withClause: Seq[WithClause] = Seq()) extends OracleStatement {
  override def toSql = Select(body, withClause).toSql
}

case class OracleDropTable(name: Name, ifExists: Boolean, cascadeConstraints: Boolean = false, purge: Boolean = false)
    extends OracleStatement {
  override def toSql = {
    val base = DropTable(name, ifExists).toSql.stripSuffix(";")
    val withCascade = if (cascadeConstraints) base + " CASCADE CONSTRAINTS" else base
    val withPurge = if (purge) withCascade + " PURGE" else withCascade
    withPurge + ";"
  }
}

case class OracleDropView(name: Name, ifExists: Boolean) extends OracleStatement {
  override def toSql = DropView(name, ifExists).toSql
}

case class OracleCreateIndex(name: Name, table: Name, columns: Seq[Name], unique: Boolean = false)
    extends OracleStatement {
  override def toSql = new CreateIndex(name, table, columns, unique).toSql
}

case class OracleAlterView(name: Name, action: AlterViewAction) extends OracleStatement {
  override def toSql = AlterView(name, action).toSql
}

sealed trait AlterTableAction {
  def toClause: String
}

case class AlterTableModify(modifications: Seq[ColumnModification]) extends AlterTableAction {
  override def toClause: String = s"MODIFY(${modifications.map(_.toSql).mkString(", ")})"
}

case class AlterTableRenameColumn(oldName: Name, newName: Name) extends AlterTableAction {
  override def toClause: String = s"RENAME COLUMN ${oldName.toSql} TO ${newName.toSql}"
}

case class AlterTableAdd(columns: Seq[ColumnDefinition], annotations: Seq[TableAnnotation] = Seq())
    extends AlterTableAction {
  override def toClause: String = s"ADD ${(columns.map(_.toSql) ++ annotations.map(_.toSql)).mkString(", ")}"
}

case class AlterTableDropColumn(column: Name) extends AlterTableAction {
  override def toClause: String = s"DROP COLUMN ${column.toSql}"
}

case class AlterTableDropColumns(columns: Seq[Name]) extends AlterTableAction {
  override def toClause: String = s"DROP (${columns.map(_.toSql).mkString(", ")})"
}

case class AlterTableDropConstraint(constraint: Name) extends AlterTableAction {
  override def toClause: String = s"DROP CONSTRAINT ${constraint.toSql}"
}

case class AlterTableAddConstraint(constraintName: Name, constraint: TableConstraint) extends AlterTableAction {
  override def toClause: String = s"ADD CONSTRAINT ${constraintName.toSql} ${constraint.toSql}"
}

sealed trait TableConstraint extends ToSql

case class ForeignKeyConstraint(
  columns: Seq[Name],
  referencesTable: Name,
  referencesColumns: Seq[Name],
  onDelete: Option[ReferentialAction] = None,
  onUpdate: Option[ReferentialAction] = None
) extends TableConstraint {
  override def toSql: String = {
    val onDeleteStr = onDelete.map(a => s" ON DELETE ${a.toSql}").getOrElse("")
    val onUpdateStr = onUpdate.map(a => s" ON UPDATE ${a.toSql}").getOrElse("")
    s"FOREIGN KEY (${columns.map(_.toSql).mkString(", ")}) REFERENCES ${referencesTable.toSql}(${referencesColumns.map(_.toSql).mkString(", ")})$onDeleteStr$onUpdateStr"
  }
}

case class UniqueConstraint(columns: Seq[Name]) extends TableConstraint {
  override def toSql: String = s"UNIQUE (${columns.map(_.toSql).mkString(", ")})"
}

case class PrimaryKeyConstraint(columns: Seq[Name]) extends TableConstraint {
  override def toSql: String = s"PRIMARY KEY (${columns.map(_.toSql).mkString(", ")})"
}

sealed class ReferentialAction(val sql: String) extends ToSql {
  override def toSql: String = sql
}

object ReferentialAction {
  case object NoAction extends ReferentialAction("NO ACTION")
  case object Restrict extends ReferentialAction("RESTRICT")
  case object Cascade extends ReferentialAction("CASCADE")
  case object SetNull extends ReferentialAction("SET NULL")
  case object SetDefault extends ReferentialAction("SET DEFAULT")
}

case class OracleAlterTable(name: Name, action: AlterTableAction) extends OracleStatement {
  override def toSql: String = s"ALTER TABLE ${name.toSql} ${action.toClause};"
}

case class ColumnModification(
  column: Name,
  dataType: Option[Name], // Optional for MODIFY column NULL
  annotations: Seq[ColumnAnnotation]
) extends ToSql {
  override def toSql = {
    val typeStr = dataType.map(" " + _.toSql).getOrElse("")
    val anns = annotations.map(_.toSql).mkString(" ")
    s"${column.toSql}$typeStr $anns".trim
  }
}

case class OracleExplain(query: SelectBody) extends OracleStatement {
  override def toSql = Explain(query).toSql
}

case class OracleCreateView(
  name: Name,
  orReplace: Boolean,
  query: SelectBody,
  materialized: Boolean = false,
  temporary: Boolean = false,
  force: Boolean = false,
  editionable: Boolean = false,
  columnList: Option[Seq[Name]] = None
) extends OracleStatement {
  override def toSql = {
    val forceStr = if (force) " FORCE" else ""
    val editionableStr = if (editionable) " EDITIONABLE" else ""
    val colListStr = columnList.map(cols => s" (${cols.map(_.toSql).mkString(", ")})").getOrElse("")
    s"CREATE ${if (orReplace) { "OR REPLACE" }
      else { "" }}$forceStr$editionableStr VIEW ${name.toSql}$colListStr AS ${query.toSql};"
  }
}

/** Oracle CREATE TABLE statement with Oracle-specific clauses.
  */
case class OracleCreateTable(
  name: Name,
  orReplace: Boolean,
  schema: Either[SelectBody, Seq[ColumnDefinition]],
  annotations: Seq[TableAnnotation],
  // Oracle-specific clauses
  tablespace: Option[String] = None,
  pctUsed: Option[Int] = None,
  pctFree: Option[Int] = None,
  storage: Option[StorageClause] = None,
  logging: Option[Boolean] = None,
  compress: Option[Boolean] = None,
  cache: Option[Boolean] = None,
  parallel: Option[Boolean] = None,
  monitoring: Option[Boolean] = None
) extends OracleStatement {
  override def toSql = {
    val base = schema match {
      case Left(query) =>
        s"CREATE ${if (orReplace) { "OR REPLACE " }
          else { "" }}TABLE ${name.toSql} AS ${query.toSql}"
      case Right(columns) =>
        s"CREATE ${if (orReplace) { "OR REPLACE " }
          else { "" }}TABLE ${name.toSql}(" +
          (columns.map(_.toSql) ++
            annotations.map(_.toSql)).mkString(", ") +
          ")"
    }
    val clauses = Seq(
      tablespace.map(t => s"TABLESPACE $t"),
      pctUsed.map(p => s"PCTUSED $p"),
      pctFree.map(p => s"PCTFREE $p"),
      storage.map(_.toSql),
      logging.map(l => if (l) "LOGGING" else "NOLOGGING"),
      compress.map(c => if (c) "COMPRESS" else "NOCOMPRESS"),
      cache.map(c => if (c) "CACHE" else "NOCACHE"),
      parallel.map(p => if (p) "PARALLEL" else "NOPARALLEL"),
      monitoring.map(m => if (m) "MONITORING" else "NOMONITORING")
    ).flatten.mkString(" ")
    s"$base $clauses;"
  }
}

/** Oracle PROMPT statement (SQL*Plus directive).
  */
case class Prompt(message: String) extends OracleStatement {
  override def toSql = s"PROMPT $message"
}

/** Oracle COMMIT statement.
  */
case class Commit() extends OracleStatement {
  override def toSql = "COMMIT"
}

/** Oracle SET DEFINE OFF statement (SQL*Plus directive).
  */
case class SetDefineOff() extends OracleStatement {
  override def toSql = "SET DEFINE OFF"
}

/** Oracle SET SCAN OFF statement (SQL*Plus directive).
  */
case class SetScanOff() extends OracleStatement {
  override def toSql = "SET SCAN OFF"
}

/** Oracle SET SCAN ON statement (SQL*Plus directive).
  */
case class SetScanOn() extends OracleStatement {
  override def toSql = "SET SCAN ON"
}

/** Oracle STORAGE clause for CREATE TABLE.
  */
case class StorageClause(
  initial: Option[String] = None,
  next: Option[String] = None,
  minExtents: Option[Int] = None,
  maxExtents: Option[String] = None, // "UNLIMITED" or number
  pctIncrease: Option[Int] = None,
  bufferPool: Option[String] = None
) extends ToSql {
  override def toSql = {
    val options = Seq(
      initial.map(i => s"INITIAL $i"),
      next.map(n => s"NEXT $n"),
      minExtents.map(m => s"MINEXTENTS $m"),
      maxExtents.map(m => s"MAXEXTENTS $m"),
      pctIncrease.map(p => s"PCTINCREASE $p"),
      bufferPool.map(b => s"BUFFER_POOL $b")
    ).flatten.mkString(" ")
    s"STORAGE ($options)"
  }
}

/** Oracle GRANT statement.
  */
case class Grant(
  privileges: Seq[String],
  onObject: Name,
  toUsers: Seq[Name], // Multiple users supported
  withGrantOption: Boolean = false
) extends OracleStatement {
  override def toSql = {
    val grantOptStr = if (withGrantOption) " WITH GRANT OPTION" else ""
    s"GRANT ${privileges.mkString(", ")} ON ${onObject.toSql} TO ${toUsers.map(_.toSql).mkString(", ")}$grantOptStr;"
  }
}

/** Oracle REVOKE statement.
  */
case class Revoke(
  privileges: Seq[String],
  onObject: Name,
  fromUsers: Seq[Name] // Multiple users supported
) extends OracleStatement {
  override def toSql =
    s"REVOKE ${privileges.mkString(", ")} ON ${onObject.toSql} FROM ${fromUsers.map(_.toSql).mkString(", ")};"
}

/** Oracle CREATE SYNONYM statement.
  */
case class OracleCreateSynonym(name: Name, orReplace: Boolean, target: Name) extends OracleStatement {
  override def toSql =
    s"CREATE ${if (orReplace) { "OR REPLACE " }
      else { "" }}SYNONYM ${name.toSql} FOR ${target.toSql};"
}

/** Oracle DROP SYNONYM statement.
  */
case class OracleDropSynonym(name: Name, ifExists: Boolean) extends OracleStatement {
  override def toSql =
    s"DROP SYNONYM ${if (ifExists) { "IF EXISTS " }
      else { "" }}${name.toSql};"
}

/** Oracle RENAME statement (standalone, not ALTER TABLE).
  */
case class OracleRename(oldName: Name, newName: Name) extends OracleStatement {
  override def toSql =
    s"RENAME ${oldName.toSql} TO ${newName.toSql};"
}

/** PL/SQL BEGIN...END block. Content is stored as a string since it may contain procedural code.
  */
case class PlSqlBlock(content: String) extends OracleStatement {
  override def toSql = s"BEGIN\n$content\nEND;"
}

/** PL/SQL FOR...LOOP...END LOOP block.
  */
case class PlSqlForLoop(variable: Name, query: SelectBody, content: String) extends OracleStatement {
  override def toSql = s"FOR ${variable.toSql} IN (${query.toSql}) LOOP\n$content\nEND LOOP;"
}

/** PL/SQL WHILE...LOOP...END LOOP block.
  */
case class PlSqlWhileLoop(condition: String, content: String) extends OracleStatement {
  override def toSql = s"WHILE $condition LOOP\n$content\nEND LOOP;"
}

/** Oracle COMMENT ON COLUMN statement.
  */
case class Comment(columnName: Name, comment: String) extends OracleStatement {
  override def toSql = s"COMMENT ON COLUMN ${columnName.toSql} IS '$comment';"
}
