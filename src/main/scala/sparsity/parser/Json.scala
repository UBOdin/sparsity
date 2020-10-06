package sparsity.parser

import sparsity.Name
import sparsity.json._
import fastparse._, MultiLineWhitespace._
import sparsity.expression.{ Expression => SparsityExpression }
import sparsity.parser.Elements.{ keyword => Keyword }


object Json
{

  def expression[_:P]: P[Expression] = P(
    Pass ~ (
      jsonTable | 
      jsonValue 
    )
  )

  def jsonTable[_:P] = P(
    (
      Keyword("JSON_TABLE") ~/ 
      "(" ~/
      Expression.expression ~ 
      (
        Keyword("FORMAT") ~/
        Keyword("JSON")
      ).? ~
      "," ~/
      path ~
      ")" ~/
      columns
    ).map { case (value, path, columns) => 
      JsonTable(value, path, columns) 
    }
  )

  def jsonValue[_:P] = P(
    (
      Keyword("JSON_QUERY") ~/
      "(" ~/
      Expression.expression ~
      (
        Keyword("FORMAT") ~/
        Keyword("JSON")
      ).? ~
      "," ~/
      path ~
      (
        Keyword("RETURNING") ~/
        Elements.identifier
      ).? ~
      Keyword("PRETTY").? ~
      Keyword("ASCII").? ~
      queryWrapper.? ~
      ")"
    ).map { case (expr, path, dataType, wrapper) =>
      JsonQuery(
        expr, 
        path, 
        wrapper.getOrElse { NoWrapper() }, 
        dataType.getOrElse { Name("varchar2") }
      )
    }
  )

  def path[_:P]: P[Path] = P(
    (
      "$" ~/ (
          pathSubscript
        | pathArrayElement
      ).rep
    ).map { Path(_) }
  )

  def pathSubscript[_:P]: P[ObjectElement] = P(
    "." ~/ (
        "*".!.map { _ => ObjectWildcard() } 
      | Elements.identifier.map { ObjectSubscript(_) }
    ) 
  )

  def pathArrayElement[_:P]: P[ArrayElement] = P(
    "[" ~ (
      "*".!.map { _ => ArrayWildcard() }
      | (arrayIndexRange ~ (
          "," ~/ arrayIndexRange
          ).rep
        ).map { case (first, rest) =>
          rest match {
            case Seq() => first
            case _ => ArrayBlocks(first +: rest)
          }
        }
    ) ~ "]"
  )

  def arrayIndexRange[_:P]: P[ArrayElementLeaf] = P(
    (
      Elements.integer ~ (
        Keyword("TO") ~/ Elements.integer
      ).?
    ).map { 
      case (index, None) => ArrayIndex(index)
      case (from, Some(to)) => ArrayRange(from, to)
    }
  )

  def columns[_:P]: P[Seq[Column]] = P(
    (
      Keyword("COLUMNS") ~/ "(" ~/
      columnDefinition ~ ("," ~/ columnDefinition).rep ~
      ")"
    ).map { case (head, rest) => head +: rest}
  )

  def columnDefinition[_:P]: P[Column] = P(
    (
      ///////// name datatype EXISTS PATH path //////////
      /* lookahead */ &( 
        Elements.identifier ~ Elements.identifier ~
        Keyword("EXISTS") 
      ) ~/ 
      Elements.identifier ~ Elements.identifier ~
        Keyword("EXISTS") ~/ Keyword("PATH") ~/ path
    ).map { case(name, dataType, path) => 
      JsonExistsColumn(name, dataType, path)
    } | (
      ///////// name datatype FORMAT JSON wrapper PATH path //////////
      /* lookahead */ &( 
        Elements.identifier ~ Elements.identifier ~
        Keyword("FORMAT") 
      ) ~/ 
      Elements.identifier ~ Elements.identifier ~
      Keyword("FORMAT") ~/ Keyword("JSON") ~/ 
      queryWrapper.? ~
      Keyword("PATH") ~/ path 
    ).map { case (name, dataType, wrapper, path) =>
      JsonQueryColumn(name, dataType, path, wrapper.getOrElse { NoWrapper() })
    } | (
      ///////// name datatype PATH path //////////
      /* lookahead */ &( 
        Elements.identifier ~ Elements.identifier ~
        Keyword("PATH") 
      ) ~/ 
      Elements.identifier ~ Elements.identifier ~
      Keyword("PATH") ~/ path
    ).map { case (name, dataType, path) => 
      JsonValueColumn(name, dataType, path)
    } | (
      Keyword("NESTED") ~/ Keyword("PATH") ~/
      path ~ columns
    ).map { case (path, columns) => 
      NestedColumns(path, columns)
    } | (
      /* lookahead */ &( 
        Elements.identifier ~ Keyword("FOR") 
      ) ~/
      Elements.identifier ~ Keyword("FOR") ~/ Keyword("ORDINALITY")
    ).map { OrdinalityColumn(_) }
  )

  def queryWrapper[_:P]: P[QueryResultWrapper] = P(
    (
      (
        Keyword("WITHOUT")
      ).map { _ => NoWrapper() } 
      | 
      (
        Keyword("WITH") ~/
        (
          Keyword("UNCONDITIONAL").map { _ => ArrayWrapper() }
          | 
          Keyword("CONDITIONAL").map { _ => ConditionalArrayWrapper() }
        ).?.map { _.getOrElse { ArrayWrapper() } }
      )
    ) ~
    Keyword("ARRAY").? ~
    Keyword("WRAPPER")
  )
  
}