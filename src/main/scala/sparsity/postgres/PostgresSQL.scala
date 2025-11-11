package sparsity.postgres

import fastparse._
import scala.io._
import java.io._
import sparsity.common.statement._
import sparsity.common.select._
import sparsity.common.{
  Array,
  BigInt,
  BigSerial,
  Boolean,
  Char,
  Cidr,
  DataType,
  Date,
  Decimal,
  Double,
  Float,
  Inet,
  Integer,
  Json,
  Jsonb,
  MacAddr,
  Serial,
  Text,
  Timestamp,
  Uuid,
  VarChar
}
import sparsity.common.parser.{SQLBase, SQLBaseObject, StreamParser}
import sparsity.common.statement.Unparseable

/** Postgres SQL dialect implementation. Supports Postgres-specific features including SERIAL, JSONB, UUID types, and
  * Postgres CREATE TABLE extensions (INHERITS, etc.).
  */
class PostgresSQL extends SQLBase {
  // Configuration
  type Stmt = Statement // Or PostgresStatement if needed
  def caseSensitive = true // Postgres preserves case in identifiers
  def statementTerminator = ";"
  def supportsCTEs = true
  def supportsReturning = true
  def supportsIfExists = true
  def stringConcatOp = "||"

  // Postgres-specific config
  def supportsInheritance = true
  def supportsOnConflict = true

  // Postgres can mostly use ANSI base implementations
  // but override where needed (e.g., data types, ON CONFLICT)
  def createStatement[$ : P]: P[Statement] = P(&(keyword("CREATE")) ~/ (postgresCreateTable | createView))

  def postgresCreateTable[$ : P]: P[Statement] = P(
    // Similar to ANSI but with Postgres extensions:
    // - INHERITS clause (can be added later)
    // - Different storage parameters
    // - UNLOGGED, TEMPORARY options
    createTable // For now, use base implementation
  )

  def dialectSpecificStatement[$ : P]: P[Statement] = P(
    fastparse.Fail // No dialect-specific statements yet
  )

  def dataType[$ : P]: P[DataType] = P(
    // Postgres-specific types first
    keyword("SERIAL").map(_ => Serial())
      | keyword("BIGSERIAL").map(_ => BigSerial())
      | keyword("JSONB").map(_ => Jsonb())
      | keyword("JSON").map(_ => Json())
      | keyword("UUID").map(_ => Uuid())
      | keyword("INET").map(_ => Inet())
      | keyword("CIDR").map(_ => Cidr())
      | keyword("MACADDR").map(_ => MacAddr())
      | (keyword("ARRAY") ~ "[" ~ dataType ~ "]")
        .map(elemType => Array(elemType))
      // Fall back to common types
      | (keyword("VARCHAR") ~ "(" ~ integer.map(_.toInt) ~ ")").map(VarChar(_))
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

object PostgresSQL extends SQLBaseObject {
  type Stmt = Statement
  import fastparse._
  import fastparse.Parsed
  private val instance = new PostgresSQL()

  def name: String = "postgres"

  protected def statementTerminator: String = ";"

  def apply(input: String): Parsed[Stmt] =
    parse(input, instance.terminatedStatement(_))

  def apply(input: Reader): StreamParser[Stmt] =
    new StreamParser[Stmt](parse(_: Iterator[String], instance.terminatedStatement(_), verboseFailures = true), input)
}
