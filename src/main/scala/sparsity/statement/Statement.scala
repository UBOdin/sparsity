package sparsity.statement

import sparsity.Name
import sparsity.expression.Expression
import sparsity.select.SelectBody
import sparsity.alter._

sealed abstract class Statement;

case class Select(
  body: SelectBody, 
  withClause: Seq[WithClause] = Seq()
) extends Statement
{
  override def toString() = (
    (if(withClause.isEmpty){ "" }
    else { "WITH "+withClause.mkString(",\n")+"\n" }) +
    body.toString+";"
  )
}

case class Update(
  table: Name, 
  set: Seq[(Name, Expression)],
  where: Option[Expression]
) extends Statement
{
  override def toString() =
    s"UPDATE $table SET ${set.map { x => x._1.toString + " = " + x._2 }.mkString(", ")}"+
    where.map { " WHERE "+_ }.getOrElse("")+";"
}

case class Delete(
  table: Name, 
  where: Option[Expression]
) extends Statement
{
  override def toString() =
    s"DELETE FROM $table"+
    where.map { " WHERE "+_ }.getOrElse("")+";"
}

case class Insert(
  table: Name, 
  columns: Option[Seq[Name]], 
  values: InsertValues,
  orReplace: Boolean
) extends Statement
{
  override def toString() =
    s"INSERT${if(orReplace){ " OR REPLACE " }else{""}} INTO $table"+
    columns.map { "("+_.mkString(", ")+")" }.getOrElse("")+
    " "+values.toString+";"

}

case class CreateTable(
  name: Name,
  orReplace: Boolean,
  columns: Seq[ColumnDefinition],
  annotations: Seq[TableAnnotation]
) extends Statement
{
  override def toString() =
    s"CREATE ${if(orReplace){"OR REPLACE "}else{""}}TABLE $name("+
    (columns.map { _.toString } ++ 
      annotations.map { _.toString }).mkString(", ")+
    ");"
}

case class CreateView(
  name: Name,
  orReplace: Boolean,
  query: SelectBody,
  materialized: Boolean = false
) extends Statement
{
  override def toString() = (
    "CREATE "+
    (if(orReplace){"OR REPLACE "} else {""})+
    (if(materialized){"MATERIALIZED "} else {""})+
    s"VIEW $name AS $query;"
  )
}

case class AlterView(
  name: Name,
  action: AlterViewAction
) extends Statement
{
  override def toString = s"ALTER VIEW $name $action"
}


case class DropTable(
  name: Name,
  ifExists: Boolean
) extends Statement
{
  override def toString() =
    "DROP TABLE "+(
      if(ifExists){ "IF EXISTS "}else{""}
    )+name+";"
}

case class DropView(
  name: Name,
  ifExists: Boolean
) extends Statement
{
  override def toString() =
    "DROP VIEW "+(
      if(ifExists){ "IF EXISTS "}else{""}
    )+name+";"
}

case class Explain(
  query: SelectBody
) extends Statement
{
  override def toString() = s"EXPLAIN $query;"
}