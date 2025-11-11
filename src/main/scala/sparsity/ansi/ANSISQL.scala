package sparsity.ansi

import fastparse._
import scala.io._
import java.io._
import sparsity.common.statement._
import sparsity.common.{
  BigInt,
  Boolean,
  Char,
  DataType,
  Date,
  Decimal,
  Double,
  Float,
  Integer,
  Text,
  Timestamp,
  VarChar
}
import sparsity.common.parser.{SQLBase, SQLBaseObject, StreamParser}
import sparsity.common.statement.Unparseable

/** ANSI SQL dialect implementation. Provides baseline SQL parsing with standard ANSI SQL features.
  */
class ANSISQL extends SQLBase {
  // Configuration
  type Stmt = Statement
  def caseSensitive = false
  def statementTerminator = ";"
  def supportsCTEs = true
  def supportsReturning = false
  def supportsIfExists = true
  def stringConcatOp = "||"

  // Parser implementations
  def createStatement[$ : P]: P[Statement] = P(&(keyword("CREATE")) ~/ (createTable | createView))

  def dialectSpecificStatement[$ : P]: P[Statement] = P(fastparse.Fail)

  def dataType[$ : P]: P[DataType] = P(
    (keyword("VARCHAR") ~ "(" ~ integer.map(_.toInt) ~ ")").map(VarChar(_))
      | (keyword("CHAR") ~ "(" ~ integer.map(_.toInt) ~ ")").map(Char(_))
      | keyword("INTEGER").map(_ => Integer())
      | keyword("INT").map(_ => Integer())
      | keyword("BIGINT").map(_ => BigInt())
      | (keyword("DECIMAL") ~ "(" ~ integer.map(_.toInt) ~ "," ~ integer.map(_.toInt) ~ ")")
        .map { case (p, s) => Decimal(p, s) }
      | keyword("FLOAT").map(_ => Float())
      | keyword("DOUBLE").map(_ => Double())
      | keyword("DATE").map(_ => Date())
      | (keyword("TIMESTAMP") ~ ("(" ~ integer.map(_.toInt) ~ ")").?)
        .map(p => Timestamp(p))
      | keyword("BOOLEAN").map(_ => Boolean())
      | keyword("TEXT").map(_ => Text())
  )
}

object ANSISQL extends SQLBaseObject {
  type Stmt = Statement
  import fastparse.Parsed
  val instance = new ANSISQL()

  def name: String = "ansi"

  protected def statementTerminator: String = ";"

  def apply(input: String): Parsed[Stmt] =
    parse(input, instance.terminatedStatement(_))

  def apply(input: Reader): StreamParser[Stmt] =
    new StreamParser[Stmt](parse(_: Iterator[String], instance.terminatedStatement(_), verboseFailures = true), input)
}
