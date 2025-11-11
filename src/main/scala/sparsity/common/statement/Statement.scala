package sparsity.common.statement

import sparsity.common.Name
import sparsity.common.expression.Expression
import sparsity.common.select.SelectBody
import sparsity.common.alter._

abstract class Statement {
  def toSql: String
}

/** Empty statement (whitespace/comments only). Used for comment-only statements or empty statements with just
  * terminators.
  */
case class EmptyStatement() extends Statement {
  override def toSql = ""
}

/** Unparseable statement (parse error recovery). Captures statements that failed to parse, allowing the parser to
  * continue.
  */
case class Unparseable(content: String) extends Statement {
  override def toSql = content
}

case class Select(body: SelectBody, withClause: Seq[WithClause] = Seq()) extends Statement {
  override def toSql =
    (if (withClause.isEmpty) { "" }
     else { "WITH " + withClause.map(_.toSql).mkString(",\n") + "\n" }) +
      body.toSql + ";"
}

case class Update(table: Name, set: Seq[(Name, Expression)], where: Option[Expression]) extends Statement {
  override def toSql =
    s"UPDATE ${table.toSql} SET ${set.map(x => x._1.toSql + " = " + x._2.toSql).mkString(", ")}" +
      where.map(w => " WHERE " + w.toSql).getOrElse("") + ";"
}

case class Delete(table: Name, where: Option[Expression]) extends Statement {
  override def toSql =
    s"DELETE FROM ${table.toSql}" +
      where.map(w => " WHERE " + w.toSql).getOrElse("") + ";"
}

case class Insert(table: Name, columns: Option[Seq[Name]], values: InsertValues, orReplace: Boolean) extends Statement {
  override def toSql =
    s"INSERT${if (orReplace) { " OR REPLACE " }
      else { "" }} INTO ${table.toSql}" +
      columns.map("(" + _.map(_.toSql).mkString(", ") + ")").getOrElse("") +
      " " + values.toSql + ";"

}

case class CreateTable(
  name: Name,
  orReplace: Boolean,
  columns: Seq[ColumnDefinition],
  annotations: Seq[TableAnnotation]
) extends Statement {
  override def toSql =
    s"CREATE ${if (orReplace) { "OR REPLACE " }
      else { "" }}TABLE ${name.toSql}(" +
      (columns.map(_.toSql) ++
        annotations.map(_.toSql)).mkString(", ") +
      ");"
}

case class CreateTableAs(name: Name, orReplace: Boolean, query: SelectBody) extends Statement {
  override def toSql =
    s"CREATE ${if (orReplace) { "OR REPLACE " }
      else { "" }}TABLE ${name.toSql} AS ${query.toSql};"
}

case class CreateView(
  name: Name,
  orReplace: Boolean,
  query: SelectBody,
  materialized: Boolean = false,
  temporary: Boolean = false
) extends Statement {
  override def toSql =
    "CREATE " +
      (if (orReplace) { "OR REPLACE " }
       else { "" }) +
      (if (materialized) { "MATERIALIZED " }
       else { "" }) +
      s"VIEW ${name.toSql} AS ${query.toSql};"
}

case class AlterView(name: Name, action: AlterViewAction) extends Statement {
  override def toSql = s"ALTER VIEW ${name.toSql} ${action.toSql}"
}

case class DropTable(name: Name, ifExists: Boolean) extends Statement {
  override def toSql =
    "DROP TABLE " + (
      if (ifExists) { "IF EXISTS " }
      else { "" }
    ) + name.toSql + ";"
}

case class DropView(name: Name, ifExists: Boolean) extends Statement {
  override def toSql =
    "DROP VIEW " + (
      if (ifExists) { "IF EXISTS " }
      else { "" }
    ) + name.toSql + ";"
}

case class Explain(query: SelectBody) extends Statement {
  override def toSql = s"EXPLAIN ${query.toSql};"
}

class CreateIndex(val name: Name, val table: Name, val columns: Seq[Name], val unique: Boolean = false)
    extends Statement {
  override def toSql = {
    val uniqueStr = if (unique) "UNIQUE " else ""
    s"CREATE ${uniqueStr}INDEX ${name.toSql} ON ${table.toSql}(${columns.map(_.toSql).mkString(", ")});"
  }
}

/** Result of parsing a single statement with position information */
case class StatementParseResult(result: Either[Unparseable, Statement], startPos: Int, endPos: Int)
