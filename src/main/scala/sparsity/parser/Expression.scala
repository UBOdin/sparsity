package sparsity.parser

import fastparse._, MultiLineWhitespace._
import sparsity.Name
import sparsity.expression._
import sparsity.parser.Elements.{keyword => Keyword}

object Expression
{
  def apply(input: String): Expression =
    parse(input, expression(_)) match { 
      case Parsed.Success(statement, index) => statement
      case f:Parsed.Failure => throw new ParseException(f)
    }

  def expressionList[_:P] = P( expression.rep(sep = Elements.comma) )

  def expression[_:P]: P[Expression] = P( disjunction )

  def disjunction[_:P] = P(
    (conjunction ~ (!(Keyword("ORDER")) ~ Keyword("OR") ~ conjunction).rep).map 
      { x => x._2.fold(x._1) { 
        (accum, current) => Arithmetic(accum, Arithmetic.Or, current)
      }
    }
  )
  def conjunction[_:P] = P(
    (negation ~ (Keyword("AND") ~ negation).rep).map 
      { x => x._2.fold(x._1) { 
        (accum, current) => Arithmetic(accum, Arithmetic.And, current)
      }
    }
  )

  def negation[_:P] = P(
    (Keyword("NOT").!.? ~ comparison).map {
      case (None, expression) => expression
      case (_, expression) => Not(expression)
    }
  )

  def comparison[_:P] = P(
    (isNullBetweenIn ~ 
      (  StringInIgnoreCase(
          "=" , "==", "!=", "<>", ">" , "<" , ">=", "<=", 
          "LIKE", "NOT LIKE",
          "RLIKE", "NOT RLIKE"
        ).! ~
        addSub
      ).?
    ).map { 
      case (expression, None) => expression
      case (lhs, Some( (op, rhs) )) => Comparison(lhs, op, rhs)
    }
  )

  def optionalNegation[_:P]: P[Expression => Expression] = P(
    Keyword("NOT").!.?.map { 
      x => if(x.isDefined) { y => Not(y) } 
           else { y => y}
    }
  )

  def isNullBetweenIn[_:P] = P(
    (addSub ~ (
      

      // IS [NOT] NULL -> IsNull(...)
      ( Keyword("IS") ~ optionalNegation ~ 
        Keyword("NULL").map 
          { _ => IsNull(_) } 
      ) | (
        // [IS] [NOT] BETWEEN low AND high 
        Keyword("IS").? ~ optionalNegation ~
        ( 
          Keyword("BETWEEN") ~ 
            addSub ~ Keyword("AND") ~ addSub
        ).map { case (low, high) =>
            { 
              (lhs:Expression) => Arithmetic(
                Comparison(lhs, Comparison.Gte, low),
                Arithmetic.And,
                Comparison(lhs, Comparison.Lte, high)
              ) 
            }
        }
      ) | (
        optionalNegation ~ Keyword("IN") ~/ (
          (
            // IN ( SELECT ... )
            &( "(" ~ Keyword("SELECT")) ~/ 
            "(" ~/ SQL.select.map {
              query => InExpression(_:Expression, Right(query))
            } ~ ")"
          ) | (
            // IN ('list', 'of', 'items')
            "(" ~/
            expressionList.map { exprs => InExpression(_:Expression, Left(exprs)) } ~
            ")"
          )
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
    Json.expression.map { JsonExpression(_) } |
    primitive | 
    jdbcvar |
    caseWhen | ifThenElse |
    cast |
    nullLiteral |
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
    | Keyword("TRUE").map { _ => BooleanPrimitive(true) }
    | Keyword("FALSE").map { _ => BooleanPrimitive(false) }
  )

  def column[_:P] = P(Elements.dottedPair.map { x => Column(x._2, x._1) })

  def nullLiteral[_:P] = P(Keyword("NULL").map { _ =>  NullPrimitive() })

  def function[_:P] = P(
    (Elements.identifier ~ "(" ~/ 
      Keyword("DISTINCT").!.?.map { _ != None } ~ 
      ( "*".!.map { _ => None }
        | expressionList.map { Some(_) }
      ) ~ ")"
    ).map { case (name, distinct, args) => 
      Function(name, args, distinct) 
    }
  )

  def jdbcvar[_:P] = P( "?".!.map { _ => JDBCVar() } )

  def caseWhen[_:P] = P(
    Keyword("CASE") ~/
    ( !Keyword("WHEN") ~ expression ).? ~
    (
      Keyword("WHEN") ~/
      expression ~
      Keyword("THEN") ~/
      expression
    ).rep ~
    Keyword("ELSE") ~/
    expression ~
    Keyword("END")
  ).map { 
    case (target, whenThen, orElse) => CaseWhenElse(target, whenThen, orElse)
  }

  def ifThenElse[_:P] = P(
    Keyword("IF") ~/ 
    expression ~/
    Keyword("THEN") ~/
    expression ~/
    Keyword("ELSE") ~/
    expression ~/
    Keyword("END")
  ).map { 
    case (condition, thenClause, elseClause) => 
      CaseWhenElse(None, Seq(condition -> thenClause), elseClause)
  }

  def cast[_:P] = P(
    (
      Keyword("CAST") ~/ "(" ~/
      expression ~ Keyword("AS") ~/ 
      Elements.identifier ~ ")"
    ).map { 
      case (expression, t) => Cast(expression, t)
    }
  )
}
