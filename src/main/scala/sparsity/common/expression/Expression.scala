package sparsity.common.expression

import sparsity.common.Name
import sparsity.common.select.{OrderBy, SelectBody}

trait ToSql {
  def toSql: String
}

sealed abstract class Expression extends ToSql {
  def needsParenthesis: Boolean
  def children: Seq[Expression]
  def rebuild(newChildren: Seq[Expression]): Expression
}

object Expression {
  def parenthesize(e: Expression) =
    if (e.needsParenthesis) { "(" + e.toSql + ")" }
    else { e.toSql }
  def escapeString(s: String) =
    s.replaceAll("'", "''")
}

sealed abstract class PrimitiveValue extends Expression {
  def needsParenthesis = false
  def children: Seq[Expression] = Seq()
  def rebuild(newChildren: Seq[Expression]): Expression = this
}
case class LongPrimitive(v: Long) extends PrimitiveValue { override def toSql = v.toString }
case class DoublePrimitive(v: Double) extends PrimitiveValue { override def toSql = v.toString }
case class StringPrimitive(v: String) extends PrimitiveValue {
  override def toSql = "'" + Expression.escapeString(v.toString) + "'"
}
case class BooleanPrimitive(v: Boolean) extends PrimitiveValue {
  override def toSql = v.toString
}
case class NullPrimitive() extends PrimitiveValue { override def toSql = "NULL" }

/** A column reference 'Table.Col'
  */
case class Column(column: Name, table: Option[Name] = None) extends Expression {
  override def toSql = (table.toSeq ++ Seq(column)).map(_.toSql).mkString(".")
  def needsParenthesis = false
  def children: Seq[Expression] = Seq()
  def rebuild(newChildren: Seq[Expression]): Expression = this
}

/** Any simple binary arithmetic expression See Arithmetic.scala for an enumeration of the possibilities *not* a case
  * class to avoid namespace collisions. Arithmetic defines apply/unapply explicitly
  */
class Arithmetic(val lhs: Expression, val op: Arithmetic.Op, val rhs: Expression) extends Expression {
  override def toSql =
    Expression.parenthesize(lhs) + " " +
      Arithmetic.opString(op) + " " +
      Expression.parenthesize(rhs)
  def needsParenthesis = true
  override def equals(other: Any): Boolean =
    other match {
      case Arithmetic(otherlhs, otherop, otherrhs) =>
        lhs.equals(otherlhs) && (op == otherop) && rhs.equals(otherrhs)
      case _ => false
    }
  def children: Seq[Expression] = Seq(lhs, rhs)
  def rebuild(c: Seq[Expression]): Expression = Arithmetic(c(0), op, c(1))
}

object Arithmetic extends Enumeration {
  type Op = Value
  val Add, Sub, Mult, Div, And, Or, BitAnd, BitOr, ShiftLeft, ShiftRight = Value

  def apply(lhs: Expression, op: Op, rhs: Expression) =
    new Arithmetic(lhs, op, rhs)
  def apply(lhs: Expression, op: String, rhs: Expression) =
    new Arithmetic(lhs, fromString(op), rhs)

  def unapply(e: Arithmetic): Option[(Expression, Op, Expression)] =
    Some((e.lhs, e.op, e.rhs))

  /** Regular expresion to match any and all binary operations
    */
  def matchRegex = """\+|-|\*|/|\|\||&&|\||&""".r

  /** Convert from the operator's string encoding to its Arith.Op rep
    */
  def fromString(a: String) =
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
      case x => throw new Exception("Invalid operand '" + x + "'")
    }

  /** Convert from the operator's Arith.Op representation to a string
    */
  def opString(v: Op): String =
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

  /** Is this binary operation a boolean operator (AND/OR)
    */
  def isBool(v: Op): Boolean =
    v match {
      case And | Or => true
      case _ => false
    }

  /** Is this binary operation a numeric operator (+, -, *, /, & , |)
    */
  def isNumeric(v: Op): Boolean = !isBool(v)

}

/** Any simple comparison expression See Comparison.scala for an enumeration of the possibilities *not* a case class to
  * avoid namespace collisions. Comparison defines apply/unapply explicitly
  */
class Comparison(val lhs: Expression, val op: Comparison.Op, val rhs: Expression) extends Expression {
  override def toSql =
    Expression.parenthesize(lhs) + " " +
      Comparison.opString(op) + " " +
      Expression.parenthesize(rhs)
  def needsParenthesis = true
  override def equals(other: Any): Boolean =
    other match {
      case Comparison(otherlhs, otherop, otherrhs) =>
        lhs.equals(otherlhs) && (op == otherop) && rhs.equals(otherrhs)
      case _ => false
    }
  def children: Seq[Expression] = Seq(lhs, rhs)
  def rebuild(c: Seq[Expression]): Expression = Comparison(c(0), op, c(1))
}
object Comparison extends Enumeration {
  type Op = Value
  val Eq, Neq, Gt, Lt, Gte, Lte, Like, NotLike, RLike, NotRLike = Value

  val strings = Map(
    "=" -> Eq,
    "==" -> Eq,
    "!=" -> Neq,
    "<>" -> Neq,
    ">" -> Gt,
    "<" -> Lt,
    ">=" -> Gte,
    "<=" -> Lte,
    "LIKE" -> Like, // SQL-style LIKE expression
    "RLIKE" -> RLike, // Regular expression lookup
    "NOT LIKE" -> NotLike, // Inverse LIKE
    "NOT RLIKE" -> RLike // Inverse NOT LIKE
  )

  def apply(lhs: Expression, op: Op, rhs: Expression) =
    new Comparison(lhs, op, rhs)
  def apply(lhs: Expression, op: String, rhs: Expression) =
    new Comparison(lhs, strings(op.toUpperCase), rhs)

  def unapply(e: Comparison): Option[(Expression, Op, Expression)] =
    Some((e.lhs, e.op, e.rhs))

  def negate(v: Op): Op =
    v match {
      case Eq => Neq
      case Neq => Eq
      case Gt => Lte
      case Gte => Lt
      case Lt => Gte
      case Lte => Gt
      case Like => NotLike
      case NotLike => Like
      case RLike => NotRLike
      case NotRLike => RLike
    }

  def flip(v: Op): Option[Op] =
    v match {
      case Eq => Some(Eq)
      case Neq => Some(Neq)
      case Gt => Some(Lt)
      case Gte => Some(Lte)
      case Lt => Some(Gt)
      case Lte => Some(Gte)
      case Like => None
      case NotLike => None
      case RLike => None
      case NotRLike => None
    }

  def opString(v: Op): String =
    v match {
      case Eq => "="
      case Neq => "<>"
      case Gt => ">"
      case Gte => ">="
      case Lt => "<"
      case Lte => "<="
      case Like => "LIKE"
      case NotLike => "NOT LIKE"
      case RLike => "RLIKE"
      case NotRLike => "NOT RLIKE"
    }
}

case class Function(name: Name, params: Option[Seq[Expression]], distinct: Boolean = false) extends Expression {
  override def toSql =
    name.toSql + "(" +
      (if (distinct) { "DISTINCT " }
       else { "" }) +
      params.map(_.map(_.toSql).mkString(", ")).getOrElse("*") +
      ")"
  def needsParenthesis = false
  def children: Seq[Expression] = params.getOrElse(Seq())
  def rebuild(c: Seq[Expression]): Expression =
    Function(name, params.map(_ => c), distinct)
}

/** Window function expression: function(...) OVER (PARTITION BY ... ORDER BY ...)
  */
case class WindowFunction(
  function: Function,
  partitionBy: Option[Seq[Expression]] = None,
  orderBy: Seq[OrderBy] = Seq()
) extends Expression {
  override def toSql = {
    val overClause = Seq("OVER") ++
      (if (partitionBy.isDefined || orderBy.nonEmpty) {
         Seq("(") ++
           partitionBy
             .map(exprs => Seq("PARTITION BY") ++ exprs.map(_.toSql))
             .getOrElse(Seq()) ++
           (if (partitionBy.isDefined && orderBy.nonEmpty) Seq(",") else Seq()) ++
           (if (orderBy.nonEmpty) Seq("ORDER BY") ++ orderBy.map(_.toSql) else Seq()) ++
           Seq(")")
       } else {
         Seq("()")
       })
    function.toSql + " " + overClause.mkString(" ")
  }
  def needsParenthesis = false
  def children: Seq[Expression] =
    function.children ++ partitionBy.getOrElse(Seq()) ++ orderBy.map(_.expression)
  def rebuild(c: Seq[Expression]): Expression = {
    val funcChildren = function.children
    val funcRebuilt = Function(function.name, function.params.map(_ => c.take(funcChildren.length)), function.distinct)
    val partitionRebuilt =
      partitionBy.map(_ => c.slice(funcChildren.length, funcChildren.length + partitionBy.get.length))
    val orderByRebuilt = orderBy.zipWithIndex.map { case (ob, idx) =>
      OrderBy(c(funcChildren.length + partitionBy.map(_.length).getOrElse(0) + idx), ob.ascending)
    }
    WindowFunction(funcRebuilt, partitionRebuilt, orderByRebuilt)
  }
}

case class JDBCVar() extends Expression {
  override def toSql = "?"
  def needsParenthesis = false
  def children: Seq[Expression] = Seq()
  def rebuild(c: Seq[Expression]): Expression = this
}

/** A scalar subquery expression: (SELECT ...) Used in contexts like VALUES clauses where a subquery can be used as a
  * value.
  */
case class Subquery(query: SelectBody) extends Expression {
  override def toSql = "(" + query.toSql + ")"
  def needsParenthesis = false
  def children: Seq[Expression] = Seq() // Subquery is a leaf expression
  def rebuild(c: Seq[Expression]): Expression = this
}

case class CaseWhenElse(target: Option[Expression], cases: Seq[(Expression, Expression)], otherwise: Expression)
    extends Expression {
  override def toSql =
    "CASE " +
      target.map(_.toSql).getOrElse("") +
      cases
        .map { clause =>
          "WHEN " + Expression.parenthesize(clause._1) +
            " THEN " + Expression.parenthesize(clause._2)
        }
        .mkString(" ") + " " +
      "ELSE " + otherwise.toSql + " END"
  def needsParenthesis = false
  def children: Seq[Expression] =
    target.toSeq ++ Seq(otherwise) ++ cases.flatMap(x => Seq(x._1, x._2))
  def rebuild(c: Seq[Expression]): Expression = {
    val (newTarget, newOtherwise, newCases) =
      if (c.length % 2 == 0) { (Some(c.head), c.tail.head, c.tail.tail) }
      else { (None, c.head, c.tail) }
    CaseWhenElse(newTarget, newCases.grouped(2).map(x => (x(0), x(1))).toSeq, newOtherwise)
  }
}

abstract class NegatableExpression extends Expression {
  def toNegatedString: String
}

case class IsNull(target: Expression) extends NegatableExpression {
  override def toSql =
    Expression.parenthesize(target) + " IS NULL"
  def toNegatedString =
    Expression.parenthesize(target) + " IS NOT NULL"
  def needsParenthesis = false
  def children: Seq[Expression] = Seq(target)
  def rebuild(c: Seq[Expression]): Expression = IsNull(c(0))
}

case class Not(target: Expression) extends Expression {
  override def toSql =
    target match {
      case neg: NegatableExpression => neg.toNegatedString
      case _ => "NOT " + Expression.parenthesize(target)
    }
  def needsParenthesis = false
  def children: Seq[Expression] = Seq(target)
  def rebuild(c: Seq[Expression]): Expression = Not(c(0))
}

case class Cast(expression: Expression, t: Name) extends Expression {
  override def toSql = "CAST(" + expression.toSql + " AS " + t.toSql + ")"
  def needsParenthesis = false
  def children: Seq[Expression] = Seq(expression)
  def rebuild(c: Seq[Expression]): Expression = Cast(c(0), t)
}

case class InExpression(expression: Expression, source: Either[Seq[Expression], SelectBody])
    extends NegatableExpression {
  override def toSql =
    Expression.parenthesize(expression) + " IN " + sourceString
  override def toNegatedString =
    Expression.parenthesize(expression) + " NOT IN " + sourceString
  def needsParenthesis = false
  def sourceString =
    source match {
      case Left(elems) => elems.map(Expression.parenthesize(_)).mkString(", ")
      case Right(query) => "(" + query.toSql + ")"
    }
  def children: Seq[Expression] =
    Seq(expression) ++ (source match {
      case Left(expr) => expr
      case Right(_) => Seq()
    })
  def rebuild(c: Seq[Expression]): Expression =
    InExpression(
      c.head,
      source match {
        case Left(_) => Left(c.tail)
        case Right(query) => Right(query)
      }
    )
}
