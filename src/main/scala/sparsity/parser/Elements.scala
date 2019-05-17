package sparsity.parser

import fastparse._, NoWhitespace._
import sparsity._

object Elements
{

  def avoidReservedKeywords[_:P] = P(
    !(
      StringInIgnoreCase(
        "FROM",
        "SELECT",
        "WHERE",
        "HAVING",
        "GROUP",
        "BY",
        "IN",
        "IS",
        "BETWEEN",
        "NOT",
        "AND",
        "OR",
        "WITH",
        "LIMIT",
        "OFFSET",
        "ALL",
        "DISTINCT",
        "UNION",
        "ORDER",
        "NATURAL",
        "INNER",
        "RIGHT",
        "LEFT",
        "OUTER",
        "JOIN",
        "ON"
      ) ~ 
      // avoid dropping keyword prefixes 
      // (e.g., 'int' matched by 'in')
      !CharIn("a-zA-Z0-9_") 
    )
  )

  def rawIdentifier[_:P] = P(
    avoidReservedKeywords ~
    (CharIn("a-zA-Z") ~ CharsWhileIn("a-zA-Z0-9_").?).!.map { Name(_) }
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