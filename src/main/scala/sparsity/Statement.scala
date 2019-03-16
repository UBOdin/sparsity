package sparsity

import sparsity.statement._

sealed abstract class Statement;

case class SelectStatement(
  body: Select, 
  withClause: Seq[WithClause] = Seq()
) extends Statement
{
  override def toString() =
    if(withClause.isEmpty){ "" }
    else { "WITH "+withClause.mkString(",\n")+"\n" } +
    body.toString+";"
}

case class UpdateStatement(
  table: Name, 
  set: Seq[(Name, Expression)],
  where: Option[Expression]
) extends Statement
{
  override def toString() =
    s"UPDATE $table SET ${set.map { x => x._1.toString + " = " + x._2 }.mkString(",")}"+
    where.map { " WHERE "+_ }.getOrElse("")+";"
}

case class DeleteStatement(
  table: Name, 
  where: Option[Expression]
) extends Statement
{
  override def toString() =
    s"DELETE FROM $table"+
    where.map { " WHERE "+_ }.getOrElse("")+";"
}

case class InsertStatement(
  table: Name, 
  columns: Option[Seq[Name]], 
  values: InsertValues,
  orReplace: Boolean
) extends Statement
{
  override def toString() =
    s"INSERT ${if(orReplace){ " OR REPLACE " }else{""}} INTO $table("+
    columns.mkString(", ")+
    s") $values;"

}

case class CreateTableStatement(
  name: Name,
  orReplace: Boolean,
  columns: Seq[ColumnDefinition],
  annotations: Seq[TableAnnotation]
) extends Statement
{
  override def toString() =
    s"CREATE ${if(orReplace){"OR REPLACE "}else{""}}TABLE $name(\n"+
    (columns.map { _.toString } ++ 
      annotations.map { _.toString }).mkString(", \n")+
    "\n);"
}

case class CreateViewStatement(
  name: Name,
  orReplace: Boolean,
  query: Select
) extends Statement
{
  override def toString() =
    s"CREATE ${if(orReplace){"OR REPLACE "}else{""}}VIEW $name AS $query;"
}

case class DropTableStatement(
  name: Name,
  ifExists: Boolean
) extends Statement
{
  override def toString() =
    "DROP TABLE "+(
      if(ifExists){ "IF EXISTS "}else{""}
    )+name+";"
}

case class DropViewStatement(
  name: Name,
  ifExists: Boolean
) extends Statement
{
  override def toString() =
    "DROP VIEW "+(
      if(ifExists){ "IF EXISTS "}else{""}
    )+name+";"
}
