package sparsity.parser

import fastparse._, NoWhitespace._
import sparsity._

object Elements
{
  def anyKeyword[_:P] = P(
    StringInIgnoreCase(
      "ALL",
      "ALTER",
      "AND",
      "ARRAY",
      "AS",
      "ASC",
      "BETWEEN",
      "BY",
      "CASE",
      "CAST",
      "COLUMNS",
      "CONDITIONAL",
      "CREATE",
      "DEFAULT",
      "DELETE",
      "DESC",
      "DISTINCT",
      "DROP",
      "ELSE",
      "END",
      "EXISTS",
      "EXPLAIN",
      "FALSE",
      "FOR",
      "FORMAT",
      "FROM",
      "FULL",
      "GROUP",
      "HAVING",
      "IF",
      "IN",
      "INDEX",
      "INNER",
      "INSERT",
      "INTO",
      "IS",
      "JOIN",
      "JSON",
      "JSON_QUERY",
      "JSON_TABLE",
      "JSON_VALUE",
      "JSON_EXISTS",
      "KEY",
      "LEFT",
      "LIMIT",
      "MATERIALIZE",
      "MATERIALIZED",
      "NATURAL",
      "NESTED",
      "NOT",
      "NULL",
      "OFFSET",
      "ON",
      "OR",
      "ORDER",
      "ORDINALITY",
      "OUTER",
      "PATH",
      "PRIMARY",
      "REPLACE",
      "RIGHT",
      "SELECT",
      "SET",
      "TABLE",
      "TEMPORARY",
      "THEN",
      "TO",
      "TRUE",
      "UNCONDITIONAL",
      "UNION",
      "UPDATE",
      "VALUES",
      "VIEW",
      "WHEN",
      "WHERE",
      "WITH",
      "WRAPPER",
      // avoid dropping keyword prefixes 
      // (e.g., 'int' matched by 'in')
    ).! ~ !CharIn("a-zA-Z0-9_") 
  )

  def keyword[_:P](expected: String*) = P[Unit](
    anyKeyword.opaque(expected.mkString(" or "))
              .filter { kw => expected.exists { _.equalsIgnoreCase(kw) } }
              .map { _ => () }
  )

  def avoidReservedKeywords[_:P] = P(
    !anyKeyword 
  )

  def rawIdentifier[_:P] = P(
    avoidReservedKeywords ~
    (CharIn("_a-zA-Z") ~ CharsWhileIn("a-zA-Z0-9_").?).!.map { Name(_) }
  )
  def quotedIdentifier[_:P] = P(
    ( ("`" ~/ CharsWhile( _ != '`' ).! ~ "`")
    | ("\"" ~/ CharsWhile( _ != '"' ).! ~ "\"")
    ).map { Name(_, true) }
  )
  def identifier[_:P]: P[Name] = P( rawIdentifier | quotedIdentifier )

  def dottedPair[_:P]: P[(Option[Name],Name)] = P(
    (identifier ~ ("." ~ identifier).?).map { 
      case (x, None)    => (None, x)
      case (x, Some(y)) => (Some(x), y)
    }
  )
  def dottedWildcard[_:P]: P[Name] = P(
    identifier ~ ".*"
  )
  def digits[_:P] = P( CharsWhileIn("0-9") )
  def plusMinus[_:P] = P( "-" | "+" )
  def integral[_:P] = ("0" | CharIn("1-9") ~ digits.?)

  def integer[_:P] = (plusMinus.? ~ digits).!.map { _.toLong } ~ !(".") // Fail on a trailing period
  def decimal[_:P] = (plusMinus.? ~ digits ~ ("." ~ digits).? ~ ("e"~plusMinus.? ~ digits).?).!.map { _.toDouble }

  def escapeQuote[_: P] = P( ("''").!.map { _.replaceAll("''", "'") } )
  def escapedString[_:P] = P( ( CharsWhile( _ != '\'' ) | escapeQuote ).rep.!.map { _.replaceAll("''", "'") } )
  def quotedString[_:P] = P("'" ~ escapedString ~ "'")

  def whitespace[_:P] = CharIn(" \n\t\r").rep
  def comma[_:P] = P("," ~ whitespace)
}