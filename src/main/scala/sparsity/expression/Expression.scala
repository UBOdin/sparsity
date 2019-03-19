package sparsity.expression

import sparsity.Name

sealed abstract class Expression
  { def needsParenthesis: Boolean }

object Expression
{
  def parenthesize(e: Expression) = 
    if(e.needsParenthesis) { "(" +e.toString +")" } else { e.toString }
  def escapeString(s: String) = 
    s.replaceAll("'", "''")
}

sealed abstract class PrimitiveValue extends Expression
{
  def needsParenthesis = false
}
case class LongPrimitive(v: Long) extends PrimitiveValue
  { override def toString = v.toString }
case class DoublePrimitive(v: Double) extends PrimitiveValue
  { override def toString = v.toString }
case class StringPrimitive(v: String) extends PrimitiveValue
  { override def toString = v.toString }
case class BooleanPrimitive(v: Boolean) extends PrimitiveValue
  { override def toString = "'"+Expression.escapeString(v.toString)+"'" }
case class NullPrimitive() extends PrimitiveValue
  { override def toString = "NULL" }

/**
 * A column reference 'Table.Col'
 */
case class Column(column: Name, table: Option[Name] = None) extends Expression
{ 
  override def toString = (table.toSeq ++ Seq(column)).mkString(".") 
  def needsParenthesis = false
}
/**
 * Any simple binary arithmetic expression
 * See Arithmetic.scala for an enumeration of the possibilities
 * *not* a case class to avoid namespace collisions.  
 * $Arithmetic defines apply/unapply explicitly
 */
class Arithmetic(
  val lhs: Expression, 
  val op: Arithmetic.Op, 
  val rhs: Expression
) extends Expression
{ 
  override def toString = 
    Expression.parenthesize(lhs)+" "+
    Arithmetic.opString(op)+" "+
    Expression.parenthesize(rhs)
  def needsParenthesis = true
  override def equals(other:Any): Boolean =
    other match {
      case Arithmetic(otherlhs, otherop, otherrhs) => lhs.equals(otherlhs) && (op == otherop) && rhs.equals(otherrhs)
      case _ => false
    }
}

object Arithmetic extends Enumeration {
  type Op = Value
  val Add, Sub, Mult, Div, And, Or, BitAnd, BitOr, ShiftLeft, ShiftRight = Value
  
  def apply(lhs: Expression, op: Op, rhs: Expression) =
    new Arithmetic(lhs, op, rhs)
  def apply(lhs: Expression, op: String, rhs: Expression) =
    new Arithmetic(lhs, fromString(op), rhs)

  def unapply(e:Arithmetic): Option[(Expression, Op, Expression)] =
    Some( (e.lhs, e.op, e.rhs) )

  /**
   * Regular expresion to match any and all binary operations
   */
  def matchRegex = """\+|-|\*|/|\|\||&&|\||&""".r

  /**
   * Convert from the operator's string encoding to its Arith.Op rep
   */
  def fromString(a: String) = {
    a.toUpperCase match {
      case "+" => Add
      case "-" => Sub
      case "*" => Mult
      case "/" => Div
      case "&" => BitAnd
      case "|" => BitOr
      case "<<" => ShiftLeft
      case ">>" => ShiftRight
      case "&&" => And
      case "||" => Or
      case "AND" => And
      case "OR" => And
      case x => throw new Exception("Invalid operand '"+x+"'")
    }
  }
  /**
   * Convert from the operator's Arith.Op representation to a string
   */
  def opString(v: Op): String = 
  {
    v match {
      case Add => "+"
      case Sub => "-"
      case Mult => "*"
      case Div => "/"
      case BitAnd => "&"
      case BitOr => "|"
      case ShiftLeft => "<<"
      case ShiftRight => ">>"
      case And => "AND"
      case Or => "OR"
    }
  }
  /**
   * Is this binary operation a boolean operator (AND/OR)
   */
  def isBool(v: Op): Boolean = 
  {
    v match {
      case And | Or => true
      case _ => false
    }
  }

  /**
   * Is this binary operation a numeric operator (+, -, *, /, & , |)
   */
  def isNumeric(v: Op): Boolean = !isBool(v)

}

/**
 * Any simple comparison expression
 * See Comparison.scala for an enumeration of the possibilities
 * *not* a case class to avoid namespace collisions.  
 * $Comparison defines apply/unapply explicitly
 */
class Comparison(
  val lhs: Expression, 
  val op: Comparison.Op, 
  val rhs: Expression
) extends Expression
{ 
  override def toString = 
    Expression.parenthesize(lhs)+" "+
    Comparison.opString(op)+" "+
    Expression.parenthesize(rhs)
  def needsParenthesis = true
  override def equals(other:Any): Boolean =
    other match {
      case Comparison(otherlhs, otherop, otherrhs) => lhs.equals(otherlhs) && (op == otherop) && rhs.equals(otherrhs)
      case _ => false
    }
}
object Comparison extends Enumeration {
  type Op = Value
  val Eq, Neq, Gt, Lt, Gte, Lte, Like, NotLike = Value

  val strings = Map(
    "="  -> Eq,
    "==" -> Eq,
    "!=" -> Neq,
    "<>" -> Neq,
    ">"  -> Gt,
    "<"  -> Lt,
    ">=" -> Gte,
    "<=" -> Lte,
    "LIKE" -> Like,
    "NOT LIKE" -> NotLike
  )
  
  def apply(lhs: Expression, op: Op, rhs: Expression) =
    new Comparison(lhs, op, rhs)
  def apply(lhs: Expression, op: String, rhs: Expression) =
    new Comparison(lhs, strings(op), rhs)

  def unapply(e:Comparison): Option[(Expression, Op, Expression)] =
    Some( (e.lhs, e.op, e.rhs) )

  def negate(v: Op): Op = 
  {
    v match {
      case Eq => Neq
      case Neq => Eq
      case Gt => Lte
      case Gte => Lt
      case Lt => Gte
      case Lte => Gt
      case Like => NotLike
      case NotLike => Like
    }
  }

  def flip(v: Op): Option[Op] = 
  {
    v match {
      case Eq      => Some(Eq)
      case Neq     => Some(Neq)
      case Gt      => Some(Lt)
      case Gte     => Some(Lte)
      case Lt      => Some(Gt)
      case Lte     => Some(Gte)
      case Like    => None
      case NotLike => None
    }
  }
  
  def opString(v: Op): String = 
  {
    v match {
      case Eq => "="
      case Neq => "<>"
      case Gt => ">"
      case Gte => ">="
      case Lt => "<"
      case Lte => "<="
      case Like => "LIKE"
      case NotLike => "NOT LIKE"
    }
  }
}

case class Function(name: Name, params: Option[Seq[Expression]], distinct: Boolean = false) extends Expression
{ 
  override def toString = 
    name.toString + "(" + 
      (if(distinct){ "DISTINCT " } else { "" }) +
      params.map{ _.mkString(", ")}.getOrElse("*")+
    ")" 
  def needsParenthesis = false
}

case class JDBCVar() extends Expression
{
  override def toString = "?"
  def needsParenthesis = false
}

case class CaseWhenElse(
  target:Option[Expression], 
  cases:Seq[(Expression, Expression)],
  otherwise:Expression
) extends Expression
{ 
  override def toString = 
  {
    "CASE "+
    target.map { _.toString }.getOrElse("")+
    cases.map { clause => 
      "WHEN "+Expression.parenthesize(clause._1)+
      " THEN "+Expression.parenthesize(clause._2) 
    }.mkString(" ")+" " +
    "ELSE "+ otherwise.toString+" END"
  }
  def needsParenthesis = false
}

abstract class NegatableExpression extends Expression
{
  def toNegatedString: String
}

case class IsNull(target: Expression) extends NegatableExpression
{
  override def toString = 
    Expression.parenthesize(target)+" IS NULL"
  def toNegatedString =
    Expression.parenthesize(target)+" IS NOT NULL"
  def needsParenthesis = false
}

case class Not(target: Expression) extends Expression
{
  override def toString = 
    target match {
      case neg:NegatableExpression => neg.toNegatedString
      case _ => "NOT "+Expression.parenthesize(target)
    }
  def needsParenthesis = false
}

case class IsBetween(lhs:Expression, low:Expression, high:Expression) extends NegatableExpression
{
  override def toString = 
    Expression.parenthesize(lhs) +
    " IS BETWEEN " +
    Expression.parenthesize(low) + 
    " AND " +
    Expression.parenthesize(high)
  def toNegatedString =
    Expression.parenthesize(lhs) +
    " IS NOT BETWEEN " +
    Expression.parenthesize(low) + 
    " AND " +
    Expression.parenthesize(high)
  def needsParenthesis = true
}

case class Cast(expression: Expression, t: String) extends Expression
{
  override def toString = "CAST("+expression.toString+" AS "+t+")"
  def needsParenthesis = false
}

case class InExpression(
  expression: Expression, 
  source: Either[Seq[Expression], sparsity.select.SelectBody]
) extends NegatableExpression
{
  override def toString = 
    Expression.parenthesize(expression) + " IN " + sourceString
  override def toNegatedString = 
    Expression.parenthesize(expression) + " NOT IN " + sourceString
  def needsParenthesis = false
  def sourceString = 
    source match {
      case Left(elems) => elems.map { Expression.parenthesize(_) }.mkString(", ")
      case Right(query) => "("+query.toString+")"
    }
}