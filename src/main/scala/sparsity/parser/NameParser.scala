package sparsity.parser

import fastparse._, NoWhitespace._
import sparsity._

object NameParser
{

  def avoidReservedKeywords[_:P] = 
    !StringInIgnoreCase(
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
      "UNION"
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
}