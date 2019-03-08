package sparsity

sealed abstract class Expression;

sealed abstract class PrimitiveValue extends Expression
case class LongPrimitive(v: Long) extends PrimitiveValue;
case class DoublePrimitive(v: Double) extends PrimitiveValue;
case class StringPrimitive(v: String) extends PrimitiveValue;
case class BooleanPrimitive(v: Boolean) extends PrimitiveValue;

case class Column(table: Option[Name], column: Name) extends Expression