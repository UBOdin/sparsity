package sparsity.common

/** Base trait for SQL data types. Supports common types shared across dialects and dialect-specific extensions.
  */
sealed trait DataType

// Common data types (shared across dialects)
case class VarChar(length: Int) extends DataType
case class Char(length: Int) extends DataType
case class Integer() extends DataType
case class BigInt() extends DataType
case class Decimal(precision: Int, scale: Int) extends DataType
case class Float() extends DataType
case class Double() extends DataType
case class Date() extends DataType
case class Timestamp(precision: Option[Int]) extends DataType
case class Boolean() extends DataType
case class Text() extends DataType

// Oracle-specific data types
case class VarChar2(length: Int, byteOrChar: Option[String]) extends DataType
case class Number(precision: Option[Int], scale: Option[Int]) extends DataType
case class Clob() extends DataType
case class Blob() extends DataType
case class NVarChar2(length: Int) extends DataType
case class Raw(length: Int) extends DataType

// Postgres-specific data types
case class Serial() extends DataType
case class BigSerial() extends DataType
case class Json() extends DataType
case class Jsonb() extends DataType
case class Uuid() extends DataType
case class Inet() extends DataType
case class Cidr() extends DataType
case class MacAddr() extends DataType
case class Array(elementType: DataType) extends DataType
