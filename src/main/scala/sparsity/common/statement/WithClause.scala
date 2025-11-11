package sparsity.common.statement

import sparsity.common.Name
import sparsity.common.select.SelectBody
import sparsity.common.expression.ToSql

case class WithClause(body: SelectBody, name: Name) extends ToSql {
  override def toSql = s"${name.toSql} AS (${body.toSql})"
}
