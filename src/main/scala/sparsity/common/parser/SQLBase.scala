package sparsity.common.parser

import fastparse._
import scala.io._
import java.io._
import sparsity.common.Name
import sparsity.common.statement._
import sparsity.common.select._
import sparsity.common.alter._
import sparsity.common.expression.{
  Arithmetic,
  BooleanPrimitive,
  CaseWhenElse,
  Cast,
  Column,
  Comparison,
  DoublePrimitive,
  Expression,
  Function,
  InExpression,
  IsNull,
  JDBCVar,
  LongPrimitive,
  Not,
  NullPrimitive,
  StringPrimitive
}
import sparsity.common.DataType

/** Base trait for SQL dialect parsers. Provides shared parser methods and configuration hooks for dialect-specific
  * behavior.
  */
trait SQLBase {
  // Type members allow dialects to extend the Statement ADT
  type Stmt <: Statement
  // Configuration for simple variations (abstract, implemented by subclasses)
  def caseSensitive: Boolean
  def statementTerminator: String // ";" or "/"
  def supportsCTEs: Boolean
  def supportsReturning: Boolean
  def supportsIfExists: Boolean
  def stringConcatOp: String // "||" or "+"
  implicit val whitespaceImplicit: fastparse.Whitespace = MultiLineWhitespace.whitespace
  // Expression parsing methods
  // These use `this` to access the SQL parser instance, removing hard-coded ANSISQL.instance references

  // Elements methods - inlined from Elements.scala to allow overriding
  def anyKeyword[$ : P] = P(
    StringInIgnoreCase(
      "ADD",
      "ALL",
      "ALTER",
      "AND",
      "AS",
      "ASC",
      "BEGIN",
      "BETWEEN",
      "BY",
      "CASE",
      "CAST",
      "COLUMN",
      "COMMENT",
      "COMMIT",
      "CONSTRAINT",
      "CREATE",
      "DECLARE",
      "DEFAULT",
      "DEFINE",
      "DELETE",
      "DESC",
      "DISABLE",
      "DISTINCT",
      "DOUBLE",
      "DROP",
      "ELSE",
      "ELSIF",
      "ENABLE",
      "END",
      "EXCEPTION",
      "EXISTS",
      "EXPLAIN",
      "FALSE",
      "FOR",
      "FROM",
      "FULL",
      "GROUP",
      "GRANT",
      "HAVING",
      "IF",
      "IN",
      "INDEX",
      "INNER",
      "INSERT",
      "INTO",
      "IS",
      "JOIN",
      "KEY",
      "LEFT",
      "LIMIT",
      "LOOP",
      "MATERIALIZE",
      "MATERIALIZED",
      "MODIFY",
      "NATURAL",
      "NOT",
      "NULL",
      "OFF",
      "OFFSET",
      "ON",
      "OR",
      "ORDER",
      "OVER",
      "OUTER",
      "PARTITION",
      "PRECISION",
      "PRIMARY",
      "PROMPT",
      "RENAME",
      "REPLACE",
      "REVERSE",
      "REVOKE",
      "RIGHT",
      "SELECT",
      "SET",
      "SMALLINT",
      "SYNONYM",
      "TABLE",
      "TEMPORARY",
      "THEN",
      "TO",
      "TRUE",
      "UNIQUE",
      "UPDATE",
      "UNION",
      "VALUES",
      "VIEW",
      "WHEN",
      "WHERE",
      "WHILE",
      "WITH",
      "FOREIGN",
      "REFERENCES",
      "CASCADE",
      "RESTRICT",
      "ACTION",
      "PURGE"
      // avoid dropping keyword prefixes
      // (e.g., 'int' matched by 'in')
    ).! ~~ !CharIn("a-zA-Z0-9_")
  )

  def keyword[$ : P](expected: String*) = P[Unit](
    anyKeyword
      .opaque(expected.mkString(" or "))
      .filter(kw => expected.exists(_.equalsIgnoreCase(kw)))
      .map(_ => ())
  )

  def avoidReservedKeywords[$ : P] = P(!anyKeyword)

  def rawIdentifier[$ : P] = P(
    avoidReservedKeywords ~~
      (CharIn("_a-zA-Z") ~~ CharsWhileIn("a-zA-Z0-9_").?).!.map(Name(_))
  )

  def quotedIdentifier[$ : P] = P(
    (("`" ~~/ CharsWhile(_ != '`').! ~ "`")
      | ("\"" ~~/ CharsWhile(_ != '"').! ~~ "\"")).map(Name(_, true: scala.Boolean))
  )

  def identifier[$ : P]: P[Name] = P(rawIdentifier | quotedIdentifier)

  def dottedPair[$ : P]: P[(Option[Name], Name)] = P((identifier ~~ ("." ~~ identifier).?).map {
    case (x, None) => (None, x)
    case (x, Some(y)) => (Some(x), y)
  })

  def dottedWildcard[$ : P]: P[Name] = P(identifier ~~ ".*")

  // Schema-qualified table name (schema.table) as a single Name
  def qualifiedTableName[$ : P]: P[Name] = P((identifier ~~ ("." ~~ identifier).?).map {
    case (x, None) => x
    case (x, Some(y)) => Name(x.name + "." + y.name, x.quoted || y.quoted)
  })

  def digits[$ : P] = P(CharsWhileIn("0-9"))
  def plusMinus[$ : P] = P("-" | "+")
  def integral[$ : P] = "0" | CharIn("1-9") ~~ digits.?

  def integer[$ : P] = (plusMinus.? ~~ digits).!.map(_.toLong) ~~ !"." // Fail on a trailing period
  def decimal[$ : P] =
    (plusMinus.? ~~ digits ~~ ("." ~~ digits).? ~~ ("e" ~~ plusMinus.? ~~ digits).?).!.map(_.toDouble)

  def escapeQuote[$ : P] = P("''".!.map(_.replaceAll("''", "'")))
  def escapedString[$ : P] = P((CharsWhile(_ != '\'') | escapeQuote).repX.!.map {
    _.replaceAll("''", "'")
  })
  def quotedString[$ : P] = P("'" ~~ escapedString ~~ "'")

  def comma[$ : P] = P(",")

  def expressionList[$ : P]: P[Seq[Expression]] = P(expression.rep(sep = comma))

  def expression[$ : P]: P[Expression] = P(disjunction)

  def disjunction[$ : P] = P((conjunction ~ (!keyword("ORDER") ~ keyword("OR") ~ conjunction).rep).map { x =>
    x._2.fold(x._1) { (accum, current) =>
      Arithmetic(accum, Arithmetic.Or, current)
    }
  })

  def conjunction[$ : P] = P((negation ~ (keyword("AND") ~ negation).rep).map { x =>
    x._2.fold(x._1) { (accum, current) =>
      Arithmetic(accum, Arithmetic.And, current)
    }
  })

  def negation[$ : P] = P((keyword("NOT").!.? ~ comparison).map {
    case (None, expression) => expression
    case (_, expression) => Not(expression)
  })

  def comparison[$ : P] = P(
    (isNullBetweenIn ~
      (StringInIgnoreCase("=", "==", "!=", "<>", ">", "<", ">=", "<=", "LIKE", "NOT LIKE", "RLIKE", "NOT RLIKE").! ~
        addSub).?).map {
      case (expression, None) => expression
      case (lhs, Some((op, rhs))) => Comparison(lhs, op, rhs)
    }
  )

  def optionalNegation[$ : P]: P[Expression => Expression] = P(keyword("NOT").!.?.map { x =>
    if (x.isDefined) { y => Not(y) }
    else { y => y }
  })

  def isNullBetweenIn[$ : P] = P(
    (addSub ~ (
      // IS [NOT] NULL -> IsNull(...)
      (keyword("IS") ~ optionalNegation ~
        keyword("NULL").map(_ => IsNull(_))) | (
        // [IS] [NOT] BETWEEN low AND high
        keyword("IS").? ~ optionalNegation ~
          (
            keyword("BETWEEN") ~
              addSub ~ keyword("AND") ~ addSub
          ).map { case (low, high) =>
            (lhs: Expression) =>
              Arithmetic(Comparison(lhs, Comparison.Gte, low), Arithmetic.And, Comparison(lhs, Comparison.Lte, high))
          }
      ) | (
        optionalNegation ~ keyword("IN") ~/ (
          (
            // IN ( SELECT ... )
            &("(" ~ keyword("SELECT")) ~/
              "(" ~/ this.select.map { query =>
                InExpression(_: Expression, Right(query))
              } ~ ")"
          ) | (
            // IN ('list', 'of', 'items')
            "(" ~/
              expressionList.map(exprs => InExpression(_: Expression, Left(exprs))) ~
              ")"
          )
        )
      )
    ).?).map {
      case (expression, None) => expression
      case (expression, Some((neg, build))) =>
        val f: Expression => Expression = neg.asInstanceOf[Expression => Expression]
        f(build(expression))
    }
  )

  def addSub[$ : P] = P((multDiv ~ ((CharIn("+\\-&\\|") | StringIn("<<", ">>")).! ~ multDiv).rep).map { x =>
    x._2.foldLeft(x._1: Expression) { (accum, current) =>
      Arithmetic(accum, current._1, current._2)
    }
  })

  def multDivOp[$ : P] = P(CharIn("*/"))

  def multDiv[$ : P] = P((leaf ~ (multDivOp.! ~ leaf).rep).map { x =>
    x._2.foldLeft(x._1) { (accum, current) =>
      Arithmetic(accum, current._1, current._2)
    }
  })

  def leaf[$ : P]: P[Expression] = P(
    parens |
      primitive |
      jdbcvar |
      caseWhen | ifThenElse |
      cast |
      nullLiteral |
      // need to lookahead `function` to avoid conflicts with `column`
      &(identifier ~ "(") ~ function |
      column
  )

  def parens[$ : P] = P("(" ~ expression ~ ")")

  def primitive[$ : P] = P(
    integer.map(v => LongPrimitive(v))
      | decimal.map(v => DoublePrimitive(v))
      | quotedString.map(v => StringPrimitive(v))
      | keyword("TRUE").map(_ => BooleanPrimitive(true))
      | keyword("FALSE").map(_ => BooleanPrimitive(false))
  )

  def column[$ : P] = P(dottedPair.map(x => Column(x._2, x._1)))

  def nullLiteral[$ : P] = P(keyword("NULL").map(_ => NullPrimitive()))

  def function[$ : P] = P(
    (identifier ~ "(" ~/
      keyword("DISTINCT").!.?.map {
        _ != None
      } ~
      ("*".!.map(_ => None)
        | expressionList.map(Some(_))) ~ ")").map { case (name, distinct, args) =>
      Function(name, args, distinct)
    }
  )

  def jdbcvar[$ : P] = P("?".!.map(_ => JDBCVar()))

  def caseWhen[$ : P] = P(
    keyword("CASE") ~/
      (!keyword("WHEN") ~ expression).? ~
      (
        keyword("WHEN") ~/
          expression ~
          keyword("THEN") ~/
          expression
      ).rep ~
      keyword("ELSE") ~/
      expression ~
      keyword("END")
  ).map { case (target, whenThen, orElse) =>
    CaseWhenElse(target, whenThen, orElse)
  }

  def ifThenElse[$ : P] = P(
    keyword("IF") ~/
      expression ~/
      keyword("THEN") ~/
      expression ~/
      keyword("ELSE") ~/
      expression ~/
      keyword("END")
  ).map { case (condition, thenClause, elseClause) =>
    CaseWhenElse(None, Seq(condition -> thenClause), elseClause)
  }

  def cast[$ : P] = P(
    (
      keyword("CAST") ~/ "(" ~/
        expression ~ keyword("AS") ~/
        identifier ~ ")"
    ).map { case (expression, t) =>
      Cast(expression, t)
    }
  )

  def statementTerminatorParser[$ : P]: P[Unit] = P(statementTerminator)

  // Parser for unparseable statements (error recovery)
  // Consumes everything up to and including the next statement terminator
  // IMPORTANT: Must match at least one char to avoid infinite loops with .rep
  def unparseableStatement[$ : P]: P[Unparseable] = P(
    // Consume at least one char, then everything up to (but not including) the terminator
    AnyChar.! ~ CharsWhile(c => c != statementTerminator.head).! ~ statementTerminatorParser.!
  ).map { case (first, rest, terminator) => Unparseable((first + rest + terminator).trim) }

  // Shared parsers with conditional logic
  // Base implementation includes unparseable fallback - can be overridden by dialects that need special handling
  def terminatedStatement[$ : P]: P[Stmt] = P(
    (statement ~/ statementTerminatorParser) |
      unparseableStatement.map(_.asInstanceOf[Stmt]) // Fallback for parse errors (requires at least one char)
  )

  def statement[$ : P]: P[Stmt] =
    P(
      Pass ~ // This trims off leading whitespace
        (parenthesizedSelect.map(s => Select(s).asInstanceOf[Stmt])
          | update
          | delete
          | insert
          | createStatement // Delegates to subclass
          | (&(keyword("ALTER")) ~/
            alterView)
          | dropTableOrView
          | explainStatement
          | dialectSpecificStatement // Hook for dialect extensions
        )
    )

  def explainStatement[$ : P]: P[Stmt] = P(keyword("EXPLAIN") ~ select.map(s => Explain(s).asInstanceOf[Stmt]))

  // Methods that can be overridden for dialect-specific behavior
  def createStatement[$ : P]: P[Stmt]
  def dialectSpecificStatement[$ : P]: P[Stmt]
  def dataType[$ : P]: P[DataType]

  // Shared logic with conditional branches
  def ifExists[$ : P]: P[Boolean] =
    if (supportsIfExists) {
      P((keyword("IF") ~ keyword("EXISTS")).!.?.map(_ != None))
    } else {
      P(Pass).map(_ => false)
    }

  def orReplace[$ : P]: P[Boolean] = P((keyword("OR") ~ keyword("REPLACE")).!.?.map(_ != None))

  def alterView[$ : P]: P[Stmt] = P(
    (
      keyword("ALTER") ~
        keyword("VIEW") ~/
        identifier ~
        (
          (keyword("MATERIALIZE").!.map(_ => Materialize(true)))
            | (keyword("DROP") ~
              keyword("MATERIALIZE").!.map(_ => Materialize(false)))
        )
    ).map { case (name, op) => AlterView(name, op).asInstanceOf[Stmt] }
  )

  def dropTableOrView[$ : P]: P[Stmt] = P(
    (
      keyword("DROP") ~
        keyword("TABLE", "VIEW").!.map(_.toUpperCase) ~/
        ifExists ~
        identifier
    ).map {
      case ("TABLE", ifExists, name) =>
        DropTable(name, ifExists).asInstanceOf[Stmt]
      case ("VIEW", ifExists, name) =>
        DropView(name, ifExists).asInstanceOf[Stmt]
      case (_, _, _) =>
        throw new Exception("Internal Error")
    }
  )

  def createView[$ : P]: P[Stmt] = P(
    (
      keyword("CREATE") ~
        orReplace ~
        keyword("MATERIALIZED", "TEMPORARY").!.?.map {
          _.map(_.toUpperCase) match {
            case Some("MATERIALIZED") => (true, false)
            case Some("TEMPORARY") => (false, true)
            case _ => (false, false)
          }
        } ~
        keyword("VIEW") ~/
        identifier ~
        keyword("AS") ~/
        select
    ).map { case (orReplace, (materialized, temporary), name, query) =>
      CreateView(name, orReplace, query, materialized, temporary).asInstanceOf[Stmt]
    }
  )

  // Parser for function calls without parentheses (e.g., CURRENT_TIMESTAMP, SYSDATE)
  def functionCallNoParens[$ : P]: P[Expression] = P(identifier.map { name =>
    // Treat as a function call with no arguments
    Function(name, None, false)
  })

  def columnAnnotation[$ : P]: P[ColumnAnnotation] = P(
    (
      keyword("PRIMARY") ~/
        keyword("KEY").map(_ => ColumnIsPrimaryKey())
    ) | (
      keyword("NOT") ~/
        keyword("NULL").map(_ => ColumnIsNotNullable())
    ) | (
      keyword("DEFAULT") ~/
        (
          ("(" ~ expression ~ ")")
            | function // Function calls with parentheses
            | functionCallNoParens // Function calls without parentheses (CURRENT_TIMESTAMP, etc.)
            | primitive
        ).map(ColumnDefaultValue(_))
    )
  )

  def oneOrMoreAttributes[$ : P]: P[Seq[Name]] = P(
    ("(" ~/ identifier.rep(sep = comma, min = 1) ~ ")")
      | identifier.map(Seq(_))
  )

  def tableField[$ : P]: P[Either[TableAnnotation, ColumnDefinition]] = P(
    (
      keyword("PRIMARY") ~/
        keyword("KEY") ~
        oneOrMoreAttributes.map(attrs => Left(TablePrimaryKey(attrs)))
    ) | (
      keyword("INDEX") ~/
        keyword("ON") ~
        oneOrMoreAttributes.map(attrs => Left(TableIndexOn(attrs)))
    ) | (
      (
        identifier ~/
          identifier ~
          ("(" ~
            primitive.rep(sep = ",") ~
            ")").?.map(_.getOrElse(Seq())) ~
          columnAnnotation.rep
      ).map { case (name, t, args, annotations) =>
        Right(ColumnDefinition(name, t, args, annotations))
      }
    )
  )

  def createTable[$ : P]: P[Stmt] = P(
    (
      keyword("CREATE") ~
        orReplace ~
        keyword("TABLE") ~/
        identifier ~
        (
          (keyword("AS") ~/ select).map(Left(_))
            | ("(" ~/
              tableField.rep(sep = comma) ~
              ")").map(Right(_))
        )
    ).map {
      case (orReplace, table, Left(query)) =>
        CreateTableAs(table, orReplace, query).asInstanceOf[Stmt]
      case (orReplace, table, Right(fields)) =>
        val columns = fields.collect { case Right(r) => r }
        val annotations = fields.collect { case Left(l) => l }
        CreateTable(table, orReplace, columns, annotations).asInstanceOf[Stmt]
    }
  )

  def valueList[$ : P]: P[InsertValues] = P(
    (
      keyword("VALUES") ~/
        ("(" ~/ expressionList ~ ")").rep(sep = comma)
    ).map(ExplicitInsert(_))
  )

  def insert[$ : P]: P[Stmt] = P(
    (
      keyword("INSERT") ~/
        (
          keyword("OR") ~/
            keyword("REPLACE")
        ).!.?.map { case None => false; case _ => true } ~
        keyword("INTO") ~/
        identifier ~
        ("(" ~/
          identifier ~
          (comma ~/ identifier).rep ~
          ")").map(x => Seq(x._1) ++ x._2).? ~
        (
          (&(keyword("SELECT")) ~/ select.map(SelectInsert(_)))
            | (&(keyword("VALUES")) ~/ valueList)
        )
    ).map { case (orReplace, table, columns, values) =>
      Insert(table, columns, values, orReplace).asInstanceOf[Stmt]
    }
  )

  def delete[$ : P]: P[Stmt] = P(
    (
      keyword("DELETE") ~/
        keyword("FROM") ~/
        identifier ~
        (
          keyword("WHERE") ~/
            expression
        ).?
    ).map { case (table, where) => Delete(table, where).asInstanceOf[Stmt] }
  )

  def update[$ : P]: P[Stmt] = P(
    (
      keyword("UPDATE") ~/
        identifier ~
        keyword("SET") ~/
        (
          identifier ~
            "=" ~/
            expression
        ).rep(sep = comma, min = 1) ~
        (
          StringInIgnoreCase("WHERE") ~/
            expression
        ).?
    ).map { case (table, set, where) =>
      Update(table, set, where).asInstanceOf[Stmt]
    }
  )

  def alias[$ : P]: P[Name] =
    P(keyword("AS").? ~ identifier)

  def selectTarget[$ : P]: P[SelectTarget] = P(
    P("*").map(_ => SelectAll())
    // Dotted wildcard needs a lookahead since a single token isn't
    // enough to distinguish between `foo`.* and `foo` AS `bar`
      | (&(dottedWildcard) ~
        dottedWildcard.map(SelectTable(_)))
      | (expression ~ alias.?).map(x => SelectExpression(x._1, x._2))
  )

  def simpleFromElement[$ : P]: P[FromElement] = P(
    (("(" ~ select ~ ")" ~ alias).map(x => FromSelect(x._1, x._2)))
      | ((dottedPair ~ alias.?).map { case (schema, table, alias) =>
        FromTable(schema, table, alias)
      })
      | (("(" ~ fromElement ~ ")" ~ alias.?).map {
        case (from, None) => from
        case (from, Some(alias)) => from.withAlias(alias)
      })
  )

  def joinWith[$ : P]: P[Join.Type] = P(
    keyword("JOIN").map(Unit => Join.Inner)
      | ((
        keyword("NATURAL").!.map(Unit => Join.Natural)
          | keyword("INNER").map(Unit => Join.Inner)
          | ((
            keyword("LEFT").map(Unit => Join.LeftOuter)
              | keyword("RIGHT").map(Unit => Join.RightOuter)
              | keyword("FULL").map(Unit => Join.FullOuter)
          ).?.map(_.getOrElse(Join.FullOuter)) ~/
            keyword("OUTER"))
      ) ~/ keyword("JOIN"))
  )

  def fromElement[$ : P]: P[FromElement] = P(
    (
      simpleFromElement ~ (
        &(joinWith) ~
          joinWith ~/
          simpleFromElement ~/
          (
            keyword("ON") ~/
              expression
          ).? ~
          alias.?
      ).rep
    ).map { case (lhs, rest) =>
      rest.foldLeft(lhs) { (lhs, next) =>
        val (t, rhs, onClause, alias) = next
        FromJoin(lhs, rhs, t, onClause.getOrElse(BooleanPrimitive(true)), alias)
      }
    }
  )

  def fromClause[$ : P] = P(
    keyword("FROM") ~/
      fromElement.rep(sep = comma, min = 1)
  )

  def whereClause[$ : P] = P(keyword("WHERE") ~/ expression)

  def groupByClause[$ : P] = P(
    keyword("GROUP") ~/
      keyword("BY") ~/
      expressionList
  )

  def havingClause[$ : P] = P(keyword("HAVING") ~ expression)

  def options[A](default: A, options: Map[String, A]): (Option[String] => A) =
    _.map(_.toUpperCase).map(options(_)).getOrElse(default)

  def ascOrDesc[$ : P] = P(keyword("ASC", "DESC").!.?.map {
    options(true, Map("ASC" -> true, "DESC" -> false))
  })

  def orderBy[$ : P] = P((expression ~ ascOrDesc).map(x => OrderBy(x._1, x._2)))

  def orderByClause[$ : P] = P(
    keyword("ORDER") ~/
      keyword("BY") ~/
      orderBy.rep(sep = comma, min = 1)
  )

  def limitClause[$ : P] = P(
    keyword("LIMIT") ~/
      integer
  )

  def offsetClause[$ : P] = P(
    keyword("OFFSET") ~/
      integer
  )

  def allOrDistinct[$ : P] = P(keyword("ALL", "DISTINCT").!.?.map {
    options(Union.Distinct, Map("ALL" -> Union.All, "DISTINCT" -> Union.Distinct))
  })

  def unionClause[$ : P] = P(keyword("UNION") ~/ allOrDistinct ~/ parenthesizedSelect)

  def parenthesizedSelect[$ : P]: P[SelectBody] = P(
    (
      "(" ~/ select ~ ")" ~/ unionClause.?
    ).map {
      case (body, Some((unionType, unionBody))) => body.unionWith(unionType, unionBody)
      case (body, None) => body
    } | select
  )

  def select[$ : P]: P[SelectBody] = P(
    (
      keyword("SELECT") ~/
        keyword("DISTINCT").!.?.map(_ != None) ~/
        selectTarget.rep(sep = ",") ~
        fromClause.?.map(_.toSeq.flatten) ~
        whereClause.? ~
        groupByClause.? ~
        havingClause.? ~
        orderByClause.?.map(_.toSeq.flatten) ~
        limitClause.? ~
        offsetClause.? ~
        unionClause.?
    ).map { case (distinct, targets, froms, where, groupBy, having, orderBy, limit, offset, union) =>
      SelectBody(
        distinct = distinct,
        target = targets,
        from = froms,
        where = where,
        groupBy = groupBy,
        having = having,
        orderBy = orderBy,
        limit = limit,
        offset = offset,
        union = union
      )
    }
  )

  // Parse multiple statements with error recovery
  // Handle End separately to avoid infinite loops
  def allStatements[$ : P]: P[Seq[Stmt]] = P(Start ~ terminatedStatement.rep ~ End).map(_.toSeq)
}

trait SQLBaseObject {
  type Stmt <: Statement
  def name: String
  def apply(input: String): Parsed[Stmt]
  def apply(input: Reader): StreamParser[Stmt]

  // Abstract method to get statement terminator for this dialect
  protected def statementTerminator: String

  // Override this to specify all valid terminator characters (e.g., both ';' and '/')
  protected def statementTerminatorChars: Set[Char] = statementTerminator.toSet

  // Parse multiple statements with error recovery, tracking positions
  def parseAll(input: String): Seq[StatementParseResult] = {
    import sparsity.common.statement.StatementParseResult
    import fastparse._
    import fastparse.Parsed

    def parseStatementsSequentially(
      remaining: String,
      currentPos: Int,
      results: List[StatementParseResult]
    ): List[StatementParseResult] = {
      // Skip whitespace and comment lines at the start
      def skipWhitespaceAndComments(s: String): (String, Int) = {
        var pos = 0
        var skipped = 0
        while (pos < s.length) {
          val char = s(pos)
          if (char.isWhitespace) {
            pos += 1
            skipped += 1
          } else if (pos + 1 < s.length && s(pos) == '-' && s(pos + 1) == '-') {
            // Skip -- comment until newline
            while (pos < s.length && s(pos) != '\n') {
              pos += 1
              skipped += 1
            }
            // Skip the newline too
            if (pos < s.length) {
              pos += 1
              skipped += 1
            }
          } else if (pos + 1 < s.length && s(pos) == '/' && s(pos + 1) == '*') {
            // Skip /* */ comment
            pos += 2
            skipped += 2
            while (pos + 1 < s.length && !(s(pos) == '*' && s(pos + 1) == '/')) {
              pos += 1
              skipped += 1
            }
            if (pos + 1 < s.length) {
              pos += 2
              skipped += 2
            }
          } else {
            // Found non-whitespace, non-comment content
            return (s.substring(pos), skipped)
          }
        }
        ("", skipped)
      }

      // Find the next statement terminator, skipping over string literals and comments
      def findNextTerminator(s: String, terminatorChars: Set[Char]): Int = {
        var pos = 0
        while (pos < s.length) {
          val char = s(pos)
          if (char == '\'') {
            // Inside string literal - skip until closing quote (handle escaped quotes '')
            pos += 1
            while (pos < s.length) {
              if (s(pos) == '\'') {
                if (pos + 1 < s.length && s(pos + 1) == '\'') {
                  pos += 2 // Skip escaped quote ''
                } else {
                  pos += 1 // End of string
                  return findNextTerminator(s.substring(pos), terminatorChars) match {
                    case -1 => -1
                    case n => pos + n
                  }
                }
              } else {
                pos += 1
              }
            }
            return -1 // Unclosed string
          } else if (pos + 1 < s.length && char == '-' && s(pos + 1) == '-') {
            // Skip -- comment until newline
            while (pos < s.length && s(pos) != '\n') pos += 1
          } else if (pos + 1 < s.length && char == '/' && s(pos + 1) == '*') {
            // Skip /* */ comment
            pos += 2
            while (pos + 1 < s.length && !(s(pos) == '*' && s(pos + 1) == '/')) pos += 1
            if (pos + 1 < s.length) pos += 2
          } else if (terminatorChars.contains(char)) {
            return pos
          } else {
            pos += 1
          }
        }
        -1
      }

      val (trimmed, skipped) = skipWhitespaceAndComments(remaining)
      val statementStartPos = currentPos + skipped

      if (trimmed.isEmpty) {
        return results.reverse
      }

      // Try to parse a single statement
      val parseResult = apply(trimmed)

      val (result, endPos) = parseResult match {
        case Parsed.Success(stmt, index) =>
          val parsedLength = index.toInt
          val statementEndPos = statementStartPos + parsedLength
          val parsedResult: Either[Unparseable, Stmt] = stmt match {
            case u: Unparseable => Left(u)
            case _ => Right(stmt.asInstanceOf[Stmt])
          }
          (parsedResult, statementEndPos)

        case Parsed.Failure(label, index, extra) =>
          // Parse failed - extract unparseable content
          val terminatorPos = findNextTerminator(trimmed, statementTerminatorChars)
          val unparseableEnd = if (terminatorPos >= 0) terminatorPos + 1 else trimmed.length

          val unparseableText = trimmed.substring(0, unparseableEnd).trim
          val unparseableEndPos = statementStartPos + unparseableEnd
          (Left(Unparseable(unparseableText)), unparseableEndPos)
      }

      val parseResultObj = StatementParseResult(result, statementStartPos, endPos)

      // Continue parsing from the end position
      val nextRemaining = if (endPos < input.length) {
        input.substring(endPos)
      } else {
        ""
      }

      parseStatementsSequentially(nextRemaining, endPos, parseResultObj :: results)
    }

    parseStatementsSequentially(input, 0, Nil)
  }
}
