package sparsity.statement

import sparsity.{Name, Expression}

sealed abstract class FromElement

case class FromTable(schema: Name, table: Name, alias: Name) extends FromElement
case class FromSelect(body: Select, alias: Name) extends FromElement
case class FromJoin(lhs: FromElement, rhs: FromElement, natural: Boolean, on: Expression) extends FromElement