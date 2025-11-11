package sparsity.common.parser

import fastparse._
import scala.io._
import java.io._
import sparsity.common.statement._
import sparsity.ansi.ANSISQL

/** Backward-compatible SQL parser object. Delegates to ANSISQL for parsing.
  *
  * @deprecated
  *   Consider using ANSISQL, OracleSQL, or PostgresSQL directly for dialect-specific parsing
  */
object SQL {
  def apply(input: String): Parsed[Statement] = ANSISQL(input)
  def apply(input: Reader): StreamParser[Statement] = ANSISQL(input)
}
