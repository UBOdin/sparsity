package sparsity

import fastparse._, MultiLineWhitespace._
import sparsity._

object ExpressionParser
{
  def apply(input: String): Expression =
    parse(input, expression(_)) match { 
      case Parsed.Success(statement, index) => statement
      case f:Parsed.Failure => throw new ParseException(f)
    }

  def expressionList[_:P] = P( expression.rep(sep = Elements.comma) )

  def expression[_:P]: P[Expression] = P( disjunction )

  def disjunction[_:P] = P(
    (conjunction ~ (StringInIgnoreCase("OR") ~ conjunction).rep).map 
      { x => x._2.fold(x._1) { 
        (accum, current) => Arithmetic(accum, Arithmetic.Or, current)
      }
    }
  )
  def conjunction[_:P] = P(
    (negation ~ (StringInIgnoreCase("AND") ~ negation).rep).map 
      { x => x._2.fold(x._1) { 
        (accum, current) => Arithmetic(accum, Arithmetic.And, current)
      }
    }
  )

  def negation[_:P] = P(
    (StringInIgnoreCase("NOT").!.? ~ comparison).map {
      case (None, expression) => expression
      case (_, expression) => Not(expression)
    }
  )

  def comparison[_:P] = P(
    (isNullBetweenIn ~ 
      (  StringInIgnoreCase(
          "=" , "==", "!=", "<>", ">" , "<" , ">=", "<=", "LIKE", "NOT LIKE"
        ).! ~
        addSub
      ).?
    ).map { 
      case (expression, None) => expression
      case (lhs, Some( (op, rhs) )) => Comparison(lhs, op, rhs)
    }
  )

  def optionalNegation[_:P]: P[Expression => Expression] = P(
    StringInIgnoreCase("NOT").!.?.map { 
      x => if(x.isDefined) { y => Not(y) } 
           else { y => y}
    }
  )

  def isNullBetweenIn[_:P] = P(
    (addSub ~ (

      (
        StringInIgnoreCase("IS") ~ optionalNegation ~ 

        // IS NULL -> IsNull(...)
        ( StringInIgnoreCase("NULL").map 
            { _ => IsNull(_) }

        // IS BETWEEN low AND high -> IsBetween(..., low, high)
        | (StringInIgnoreCase("BETWEEN") ~ addSub ~ "AND" ~ addSub).map
            { case (low, high) => IsBetween(_:Expression, low, high) }
        )
      ) | (
        optionalNegation ~ StringInIgnoreCase("IN") ~/ "(" ~/ (
          (&("SELECT") ~ SQL.select.map { 
            query => InExpression(_:Expression, Right(query))
          }) | 
          expressionList.map { exprs => InExpression(_:Expression, Left(exprs)) }
        )
      )
    ).?).map { 
      case (expression, None)        => expression
      case (expression, Some((neg, build)))   => neg( build(expression) )
    }
  )

  def addSub[_:P] = P(
    ( multDiv ~ ((CharIn("+\\-&\\|") | StringIn("<<", ">>")).! ~ multDiv).rep ).map 
      { x => 
        x._2.foldLeft(x._1:Expression) {
          (accum, current) => Arithmetic(accum, current._1, current._2)
        }
      }
  )

  def multDivOp[_:P] = P(
    CharIn("*/") | StringIn("&&", "||")
  )

  def multDiv[_:P] = P(
    ( leaf ~ (multDivOp.! ~ leaf).rep ).map
      { x => 
        x._2.foldLeft(x._1) {
          (accum, current) => Arithmetic(accum, current._1, current._2)
        }
      }
  )

  def leaf[_:P]: P[Expression] = P( 
    parens | 
    primitive | 
    jdbcvar |
    caseWhen |
    cast |
    // need to lookahead `function` to avoid conflicts with `column`
    &(Elements.identifier ~ "(") ~ function | 
    column 
  )

  def parens[_:P] = P( 
    ( "(" ~ expression ~ ")" )
  )

  def primitive[_:P] = P( 
      Elements.integer.map { v => LongPrimitive(v) } 
    | Elements.decimal.map { v => DoublePrimitive(v) }
    | Elements.quotedString.map { v => StringPrimitive(v) }
    | StringInIgnoreCase("TRUE").map { _ => BooleanPrimitive(true) }
    | StringInIgnoreCase("FALSE").map { _ => BooleanPrimitive(false) }
  )

  def column[_:P] = P(Elements.dottedPair.map { x => Column(x._2, x._1) })

  def function[_:P] = P(
    (Elements.identifier ~ "(" ~/ 
      StringInIgnoreCase("DISTINCT").!.?.map { _ != None } ~ 
      ( "*".!.map { _ => None }
        | expressionList.map { Some(_) }
      ) ~ ")"
    ).map { case (name, distinct, args) => 
      Function(name, args, distinct) 
    }
  )

  def jdbcvar[_:P] = P( "?".!.map { _ => JDBCVar() } )

  def caseWhen[_:P] = P(
    StringInIgnoreCase("CASE") ~/
    ( !StringInIgnoreCase("WHEN") ~ expression ).? ~
    (
      StringInIgnoreCase("WHEN") ~/
      expression ~
      StringInIgnoreCase("THEN") ~/
      expression
    ).rep ~
    StringInIgnoreCase("ELSE") ~/
    expression ~
    StringInIgnoreCase("END")
  ).map { 
    case (target, whenThen, orElse) => CaseWhenElse(target, whenThen, orElse)
  }

  def cast[_:P] = P(
    (
      "CAST" ~/ "(" ~/
      expression ~ "AS" ~/ 
      Elements.rawIdentifier ~ ")"
    ).map { 
      case (expression, Name(t, _)) => Cast(expression, t)
    }
  )
}
