package sparsity.oracle

import fastparse._
import fastparse.ParsingRun
import fastparse.internal.{Msgs, Util}
import scala.io._
import java.io._
import sparsity.common.statement._
import sparsity.oracle._
import sparsity.common.select.{SelectBody, _}
import sparsity.common.alter.Materialize
import sparsity.common.expression._
import sparsity.common.{
  BigInt,
  Blob,
  Boolean,
  Char,
  Clob,
  DataType,
  Date,
  Decimal,
  Double,
  Float,
  Integer,
  NVarChar2,
  Name,
  Number,
  Raw,
  Text,
  Timestamp,
  VarChar,
  VarChar2
}
import sparsity.common.parser.{SQLBase, SQLBaseObject, StreamParser}
import fastparse.CharIn

/** Custom whitespace parser that includes SQL comments (-- ... and /* ... */). This allows comments to be automatically
  * skipped anywhere whitespace is allowed. Based on FastParse's JavaWhitespace pattern.
  */
object OracleWhitespace {
  implicit object whitespace extends Whitespace {
    def apply(ctx: ParsingRun[_]) = {
      val input = ctx.input
      @scala.annotation.tailrec
      def rec(current: Int, state: Int): ParsingRun[Unit] =
        if (!input.isReachable(current)) {
          if (state == 0 || state == 2) {
            // Normal whitespace or inside -- comment - both are valid at EOF
            if (ctx.verboseFailures) ctx.reportTerminalMsg(current, Msgs.empty)
            ctx.freshSuccessUnit(current)
          } else if (state == 1 || state == 3) {
            // After first '-' or '/' but not a comment - return success at previous position
            if (ctx.verboseFailures) ctx.reportTerminalMsg(current - 1, Msgs.empty)
            ctx.freshSuccessUnit(current - 1)
          } else {
            // Inside /* */ comment but reached EOF - this is an error
            ctx.cut = true
            val res = ctx.freshFailure(current)
            if (ctx.verboseFailures) ctx.reportTerminalMsg(current, () => Util.literalize("*/"))
            res
          }
        } else {
          val currentChar = input(current)
          (state: @scala.annotation.switch) match {
            case 0 =>
              // Normal whitespace mode
              (currentChar: @scala.annotation.switch) match {
                case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
                case '-' => rec(current + 1, state = 1) // Might be start of -- comment
                case '/' => rec(current + 1, state = 3) // Might be start of /* comment
                case _ =>
                  if (ctx.verboseFailures) ctx.reportTerminalMsg(current, Msgs.empty)
                  ctx.freshSuccessUnit(current)
              }
            case 1 =>
              // After first '-', check if second '-' follows
              if (currentChar == '-') {
                rec(current + 1, state = 2) // Start of -- comment
              } else {
                // Not a comment, just a single '-', return success at previous position
                if (ctx.verboseFailures) ctx.reportTerminalMsg(current - 1, Msgs.empty)
                ctx.freshSuccessUnit(current - 1)
              }
            case 2 =>
              // Inside -- comment, consume until newline
              rec(current + 1, state = if (currentChar == '\n') 0 else state)
            case 3 =>
              // After first '/', check if '*' follows
              (currentChar: @scala.annotation.switch) match {
                case '*' => rec(current + 1, state = 4) // Start of /* comment
                case _ =>
                  // Not a comment, just a single '/', return success at previous position
                  if (ctx.verboseFailures) ctx.reportTerminalMsg(current - 1, Msgs.empty)
                  ctx.freshSuccessUnit(current - 1)
              }
            case 4 =>
              // Inside /* */ comment, waiting for '*'
              rec(current + 1, state = if (currentChar == '*') 5 else state)
            case 5 =>
              // Inside /* */ comment, after '*', checking for '/'
              (currentChar: @scala.annotation.switch) match {
                case '/' => rec(current + 1, state = 0) // End of /* comment
                case '*' => rec(current + 1, state = 5) // Stay in state 5 if another '*'
                case _ => rec(current + 1, state = 4) // Go back to state 4 if not '/'
              }
          }
        }
      rec(current = ctx.index, state = 0)
    }
  }
}

/** Oracle SQL dialect implementation. Supports Oracle-specific features including PROMPT, VARCHAR2, NUMBER types, and
  * Oracle CREATE TABLE clauses (TABLESPACE, STORAGE, etc.). For reference, see
  * https://github.com/antlr/grammars-v4/blob/master/sql/plsql/PlSqlParser.g4
  */
class OracleSQL extends SQLBase {
  // Configuration
  type Stmt = Statement
  def caseSensitive = false
  def statementTerminator = "/" // Oracle supports both "/" and ";"
  def supportsCTEs = true
  def supportsReturning = true
  def supportsIfExists = true
  def stringConcatOp = "||"

  // Oracle-specific config
  def supportsPrompt = true
  def supportsVarchar2 = true

  override implicit val whitespaceImplicit: fastparse.Whitespace = OracleWhitespace.whitespace

  // Override multDivOp to prevent "/" from being parsed as division when it's a statement terminator.
  // In Oracle, "/" on its own line is a statement terminator, not division.
  // Division "/" must be followed by an expression operand (identifier, number, paren, etc.)
  // Statement terminator "/" is followed by newline/whitespace leading to EOF or next statement.
  // We use negative lookahead: "/" is NOT division if followed by whitespace then a statement keyword.
  override def multDivOp[$ : P]: P[Unit] = P(
    "*" | ("/" ~ !(&(
      // After whitespace (handled by implicit whitespace parser), check we're NOT at:
      // - End of input
      // - A statement-starting keyword
      End |
        keyword("CREATE") | keyword("SELECT") | keyword("INSERT") | keyword("UPDATE") |
        keyword("DELETE") | keyword("DROP") | keyword("ALTER") | keyword("GRANT") |
        keyword("REVOKE") | keyword("COMMIT") | keyword("EXPLAIN") | keyword("WITH") |
        keyword("BEGIN") | keyword("DECLARE") | keyword("SET") | keyword("PROMPT") |
        keyword("RENAME") | keyword("COMMENT")
    )))
  )

  // Helper parser for digits that allows whitespace consumption
  // Note: We don't override base digits since it returns P[Unit], but we need P[String] here
  def oracleDigits[$ : P]: P[String] = P(CharsWhileIn("0-9").!)

  // Helper parser for NUMBER precision/scale that allows * for unspecified precision
  def numberPrecision[$ : P]: P[String] = P("*".! | oracleDigits)

  // Oracle-specific identifier parser that works with OracleWhitespace
  // Override base identifier to use Oracle-specific parsing
  // Note: In Oracle, keywords can be used as identifiers in many contexts (especially after dots)
  // So we allow keywords as identifiers, but prefer quoted identifiers when ambiguous
  override def identifier[$ : P]: P[Name] = P(
    (("`" ~~/ CharsWhile(_ != '`').! ~ "`")
      | ("\"" ~~/ CharsWhile(_ != '"').! ~~ "\"")).map(Name(_, true: scala.Boolean))
      | (CharIn("_a-zA-Z") ~~ CharsWhileIn("a-zA-Z0-9_").?).!.map(Name(_))
  )

  // Oracle-specific parser for table names with optional database links (schema.table@link)
  def oracleQualifiedTableName[$ : P]: P[Name] = P((identifier ~~ ("." ~/ identifier).? ~~ ("@" ~/ identifier).?).map {
    case (x: Name, y: Option[Name], link: Option[Name]) =>
      (x, y, link) match {
        case (x, None, None) => x
        case (x, Some(y), None) => Name(x.name + "." + y.name, x.quoted || y.quoted)
        case (x, None, Some(link)) => Name(x.name + "@" + link.name, x.quoted || link.quoted)
        case (x, Some(y), Some(link)) =>
          Name(x.name + "." + y.name + "@" + link.name, x.quoted || y.quoted || link.quoted)
      }
  })

  // Oracle-specific parser for dotted pairs with optional database links (schema.table@link)
  def oracleDottedPair[$ : P]: P[(Option[Name], Name)] = P(
    (identifier ~~ ("." ~/ identifier).? ~~ ("@" ~/ identifier).?).map {
      case (x: Name, y: Option[Name], link: Option[Name]) =>
        (x, y, link) match {
          case (x, None, None) => (None, x)
          case (x, Some(y), None) => (Some(x), y)
          case (x, None, Some(link)) => (None, Name(x.name + "@" + link.name, x.quoted || link.quoted))
          case (x, Some(y), Some(link)) =>
            (Some(x), Name(y.name + "@" + link.name, y.quoted || link.quoted))
        }
    }
  )

  // Override alias to use rawIdentifier which checks for keywords
  // This prevents clause keywords like WHERE from being parsed as aliases
  override def alias[$ : P]: P[Name] = P(keyword("AS").? ~ rawIdentifier)

  // Override simpleFromElement to support Oracle database links
  // Note: For subqueries, alias is optional in Oracle (unlike base class which requires it)
  override def simpleFromElement[$ : P]: P[FromElement] = P(
    (("(" ~ select ~ ")" ~ alias.?).map {
      case (body, Some(alias)) => FromSelect(body, alias)
      case (body, None) =>
        FromSelect(body, Name("")) // Anonymous alias for subqueries without alias
    })
      | ((oracleDottedPair ~ alias.?).map { case (schema, table, alias) =>
        FromTable(schema, table, alias)
      })
      | (("(" ~ fromElement ~ ")" ~ alias.?).map {
        case (from, None) => from
        case (from, Some(alias)) => from.withAlias(alias)
      })
  )

  // Override column to use oracleDottedPair for proper Oracle identifier parsing
  override def column[$ : P]: P[Column] = P(oracleDottedPair.map(x => Column(x._2, x._1)))

  // Override leaf to allow keywords as function names (e.g., REPLACE(...))
  override def leaf[$ : P]: P[Expression] = P(
    parens |
      primitive |
      jdbcvar |
      caseWhen | ifThenElse |
      cast |
      nullLiteral |
      // Window functions: function(...) OVER (...) - parse function then check for OVER
      (function ~ (keyword("OVER") ~/
        "(" ~/
        (
          (keyword("PARTITION") ~/ keyword("BY") ~/ expressionList).? ~
            (keyword("ORDER") ~/ keyword("BY") ~/ orderBy
              .rep(sep = comma, min = 1)
              .map(_.toSeq)).?
        ) ~
        ")").?).map {
        case (func, Some((partitionByOpt, orderByOpt))) =>
          WindowFunction(func, partitionByOpt, orderByOpt.getOrElse(Seq()))
        case (func, None) =>
          func
      } |
      // Try column first to avoid conflicts with quoted identifiers
      column |
      // Allow keywords as function names when followed by (
      // Use lookahead that explicitly excludes quoted identifiers (starts with " or `)
      &(!CharIn("\"`") ~ CharIn("_a-zA-Z") ~ CharsWhileIn("a-zA-Z0-9_").? ~ "(") ~ function
  )

  // Override parens to handle subqueries: (SELECT ...)
  override def parens[$ : P]: P[Expression] = P(
    // Try subquery first: (SELECT ...)
    ("(" ~/ &(keyword("SELECT")) ~/ select ~ ")").map(Subquery(_)) |
      // Fall back to regular parenthesized expression: (expression)
      ("(" ~ expression ~ ")")
  )

  // Window function parser: function(...) OVER (PARTITION BY ... ORDER BY ...)
  def windowFunction[$ : P]: P[WindowFunction] = P(
    function ~
      keyword("OVER") ~/
      "(" ~/
      (
        (keyword("PARTITION") ~/ keyword("BY") ~/ expressionList).? ~
          (keyword("ORDER") ~/ keyword("BY") ~/ orderBy
            .rep(sep = comma, min = 1)
            .map(_.toSeq)).?
      ) ~
      ")"
  ).map { case (func, (partitionByOpt, orderByOpt)) =>
    WindowFunction(func, partitionByOpt, orderByOpt.getOrElse(Seq()))
  }

  // Override function to allow keywords as function names
  // But explicitly reject quoted identifiers (they should be columns, not functions)
  override def function[$ : P]: P[Function] = P(
    // First check that we don't have a quoted identifier
    !CharIn("\"`") ~
      (identifier ~ "(" ~/
        keyword("DISTINCT").!.?.map {
          _ != None
        } ~
        ("*".!.map(_ => None)
          | expressionList.map(Some(_))) ~ ")").map { case (name, distinct, args) =>
        Function(name, args, distinct)
      }
  )

  // Parser implementations
  override def dialectSpecificStatement[$ : P]: P[OracleStatement] = P(
    prompt | commit | setScanOff | setScanOn | setDefineOff | grant | revoke | dropSynonym | rename | comment
  )

  def prompt[$ : P]: P[Prompt] = P(
    keyword("PROMPT") ~/ CharsWhile(c => c != '\n' && c != '/' && c != ';').!.map(_.trim)
      .map(Prompt(_))
  )

  def commit[$ : P]: P[Commit] = P(keyword("COMMIT") ~/ Pass.map(_ => Commit()))

  def setDefineOff[$ : P]: P[SetDefineOff] = P(
    keyword("SET") ~ &(keyword("DEFINE")) ~ keyword("DEFINE") ~ keyword("OFF") ~ Pass.map(_ => SetDefineOff())
  )

  def setScanOff[$ : P]: P[SetScanOff] = P(
    keyword("SET") ~ StringInIgnoreCase("SCAN") ~ StringInIgnoreCase("OFF") ~ Pass.map(_ => SetScanOff())
  )

  def setScanOn[$ : P]: P[SetScanOn] = P(
    keyword("SET") ~ StringInIgnoreCase("SCAN") ~ StringInIgnoreCase("ON") ~ Pass.map(_ => SetScanOn())
  )

  def grant[$ : P]: P[Grant] = P(
    keyword("GRANT") ~/
      grantPrivilegeList ~
      keyword("ON") ~/
      oracleQualifiedTableName ~
      keyword("TO") ~/
      identifier.rep(sep = comma, min = 1).map(_.toSeq) ~
      (keyword("WITH") ~/ keyword("GRANT") ~/ StringInIgnoreCase("OPTION")).!.?.map(_.isDefined)
  ).map { case (privileges, onObject, toUsers, withGrantOption) =>
    Grant(privileges, onObject, toUsers, withGrantOption)
  }

  def revoke[$ : P]: P[Revoke] = P(
    keyword("REVOKE") ~/
      grantPrivilegeList ~
      keyword("ON") ~/
      oracleQualifiedTableName ~
      keyword("FROM") ~/
      identifier.rep(sep = comma, min = 1).map(_.toSeq)
  ).map { case (privileges, onObject, fromUsers) =>
    Revoke(privileges, onObject, fromUsers)
  }

  def grantPrivilegeList[$ : P]: P[Seq[String]] = P(grantPrivilege ~ (comma ~/ grantPrivilege).rep).map {
    case (first, rest) => Seq(first) ++ rest.toSeq
  }

  def grantPrivilege[$ : P]: P[String] = P(
    // Multi-word privileges first (must come before single-word to avoid partial matches)
    // Parse "ON COMMIT REFRESH" as a single unit
    (StringInIgnoreCase("ON") ~/ StringInIgnoreCase("COMMIT") ~/ StringInIgnoreCase("REFRESH")).!.map(_ =>
      "ON COMMIT REFRESH"
    ) |
      (StringInIgnoreCase("QUERY") ~/ StringInIgnoreCase("REWRITE")).!.map(_ => "QUERY REWRITE") |
      // Single-word privileges (exclude "ON" and "QUERY" to avoid conflicts)
      StringInIgnoreCase(
        "ALTER",
        "DELETE",
        "INDEX",
        "INSERT",
        "REFERENCES",
        "SELECT",
        "UPDATE",
        "ALL",
        "DEBUG",
        "FLASHBACK"
      ).!
  )

  // Parser for Oracle type names that may be multi-word (e.g., DOUBLE PRECISION, SMALLINT)
  def oracleTypeName[$ : P]: P[Name] = P(
    (keyword("DOUBLE") ~ keyword("PRECISION")).map(_ => Name("DOUBLE PRECISION")) |
      keyword("DOUBLE").map(_ => Name("DOUBLE")) |
      keyword("SMALLINT").map(_ => Name("SMALLINT")) |
      identifier
  )

  // Override oneOrMoreAttributes to use OracleWhitespace instead of MultiLineWhitespace
  override def oneOrMoreAttributes[$ : P]: P[Seq[Name]] = P(
    ("(" ~/ identifier.rep(sep = comma, min = 1) ~ ")")
      | identifier.map(Seq(_))
  )

  // Helper parser for Oracle type parameters (NUMBER(p,s), VARCHAR2(size BYTE|CHAR), etc.)
  // Returns the parameter string (e.g., "(10,2)", "(15 BYTE)")
  def typeParameters[$ : P]: P[String] = P(
    "(" ~/
      (
        // NUMBER(*,0) or NUMBER(3,2) or NUMBER(3,2 BYTE) - precision and scale with optional unit
        // Try this FIRST to avoid backtracking issues with NUMBER(10, 2)
        (numberPrecision ~
          "," ~ numberPrecision ~
          (StringInIgnoreCase("BYTE", "CHAR").!.?)).map { case (size, scale, unit) =>
          val unitStr = unit.map(" " + _).getOrElse("")
          s"($size,$scale$unitStr)"
        } |
          // VARCHAR2(15 BYTE) or NUMBER(3) or NUMBER(3 BYTE) - size with optional unit
          // Parse digits directly to allow whitespace consumption
          (oracleDigits ~
            (StringInIgnoreCase("BYTE", "CHAR").!.?)).map { case (size, unit) =>
            val unitStr = unit.map(" " + _).getOrElse("")
            s"($size$unitStr)"
          }
      ) ~
      ")"
  ).!

  // Override tableField to handle Oracle-specific data type syntax
  override def tableField[$ : P]: P[Either[TableAnnotation, ColumnDefinition]] = P(
    (
      keyword("CONSTRAINT") ~
        identifier ~
        keyword("UNIQUE") ~/
        oneOrMoreAttributes
    ).map { case (_, attrs) => Left(TableUnique(attrs)) } | (
      keyword("CONSTRAINT") ~
        identifier ~
        keyword("PRIMARY") ~/
        keyword("KEY") ~/
        oneOrMoreAttributes
    ).map { case (_, attrs) => Left(TablePrimaryKey(attrs)) } | (
      keyword("UNIQUE") ~/
        oneOrMoreAttributes.map(attrs => Left(TableUnique(attrs)))
    ) | (
      keyword("PRIMARY") ~/
        keyword("KEY") ~/
        oneOrMoreAttributes.map(attrs => Left(TablePrimaryKey(attrs)))
    ) | (
      keyword("INDEX") ~/
        keyword("ON") ~
        oneOrMoreAttributes.map(attrs => Left(TableIndexOn(attrs)))
    ) | (
      (
        identifier ~/
          oracleTypeName ~
          typeParameters.? ~
          oracleColumnAnnotation.rep
      ).map { case (name, typeName, paramsOpt, annotations) =>
        val fullTypeName = Name(typeName.name + paramsOpt.getOrElse(""))
        Right(ColumnDefinition(name, fullTypeName, Seq(), annotations))
      }
    )
  )

  // Override columnAnnotation to handle Oracle-specific ENABLE/DISABLE keywords and DEFAULT ON NULL
  def oracleColumnAnnotation[$ : P]: P[ColumnAnnotation] = P(
    // Try DEFAULT ON NULL first (Oracle-specific) - use lookahead to check full sequence
    (&(keyword("DEFAULT") ~ keyword("ON") ~ keyword("NULL")) ~
      keyword("DEFAULT") ~/
      keyword("ON") ~/
      keyword("NULL") ~/
      (
        ("(" ~ expression ~ ")")
          | function // Function calls with parentheses
          | functionCallNoParens // Function calls without parentheses (CURRENT_TIMESTAMP, etc.)
          | primitive
      )).map(ColumnDefaultValue(_)) |
      // DEFAULT NULL (without ON) - use lookahead to ensure NULL follows
      (&(keyword("DEFAULT") ~ keyword("NULL")) ~
        keyword("DEFAULT") ~/
        keyword("NULL")).map(_ => ColumnDefaultValue(NullPrimitive())) |
      // Standalone NULL annotation (Oracle allows explicit NULL, though it's redundant)
      keyword("NULL").map(_ => ColumnIsNullable()) |
      // Fall back to standard column annotations (including regular DEFAULT and NOT NULL)
      // Oracle allows ENABLE/DISABLE followed by VALIDATE/NOVALIDATE
      columnAnnotation ~ (keyword("ENABLE") | keyword("DISABLE")).? ~ (StringInIgnoreCase(
        "VALIDATE"
      ) | StringInIgnoreCase("NOVALIDATE")).?
  )

  // Override common statements to return OracleStatement wrappers
  override def insert[$ : P]: P[OracleStatement] = P(
    (
      keyword("INSERT") ~/
        (
          keyword("OR") ~/
            keyword("REPLACE")
        ).!.?.map { case None => false; case _ => true } ~
        keyword("INTO") ~/
        oracleQualifiedTableName ~
        ("(" ~/
          identifier ~
          (comma ~/ identifier).rep ~
          ")").map(x => Seq(x._1) ++ x._2).? ~
        (
          // WITH ... SELECT
          (&(keyword("WITH")) ~/ withSelect.map { case (ctes, body) => SelectInsert(body) }) |
            // SELECT (without WITH)
            (&(keyword("SELECT")) ~/ select.map(SelectInsert(_))) |
            // VALUES
            (&(keyword("VALUES")) ~/ valueList)
        )
    ).map { case (orReplace, table, columns, values) =>
      OracleInsert(table, columns, values, orReplace)
    }
  )

  // Parser for SELECT with optional WITH clause
  def withSelect[$ : P]: P[(Seq[WithClause], SelectBody)] = P(withClauseList ~ select).map { case (ctes, body) =>
    (ctes, body)
  }

  def withClauseList[$ : P]: P[Seq[WithClause]] = P(
    keyword("WITH") ~/
      withClause.rep(sep = comma, min = 1).map(_.toSeq)
  )

  def withClause[$ : P]: P[WithClause] = P(
    identifier ~
      keyword("AS") ~/
      "(" ~/ select ~ ")"
  ).map { case (name, body) => WithClause(body, name) }

  override def update[$ : P]: P[OracleStatement] = P(
    (
      keyword("UPDATE") ~/
        oracleQualifiedTableName ~
        // Optional table alias: identifier that is not part of a dotted name and is followed by SET
        // Use negative lookahead to ensure we don't match "SET" as an alias
        (!keyword("SET") ~~ identifier).? ~
        keyword("SET") ~/
        (
          dottedPair.map { case (schema, col) =>
            val colName =
              schema.map(s => Name(s.name + "." + col.name, s.quoted || col.quoted)).getOrElse(col)
            colName
          } ~
            "=" ~/
            expression
        ).rep(sep = comma, min = 1) ~
        (
          StringInIgnoreCase("WHERE") ~/
            expression
        ).?
    ).map { case (table, _, set, where) =>
      OracleUpdate(table, set, where)
    }
  )

  override def delete[$ : P]: P[OracleStatement] = P(
    (
      keyword("DELETE") ~/
        keyword("FROM").!.?.map(_ => ()) ~/ // FROM is optional in Oracle
        oracleQualifiedTableName ~
        (
          keyword("WHERE") ~/
            expression
        ).?
    ).map { case (table, where) => OracleDelete(table, where) }
  )

  override def alterView[$ : P]: P[OracleStatement] = P(
    (
      keyword("ALTER") ~
        keyword("VIEW") ~/
        oracleQualifiedTableName ~
        (
          (keyword("MATERIALIZE").!.map(_ => Materialize(true)))
            | (keyword("DROP") ~
              keyword("MATERIALIZE").!.map(_ => Materialize(false)))
        )
    ).map { case (name, op) => OracleAlterView(name, op) }
  )

  def alterTable[$ : P]: P[OracleStatement] = P(
    (
      keyword("ALTER") ~
        keyword("TABLE") ~/
        oracleQualifiedTableName ~
        (
          alterTableModify |
            alterTableRenameColumn |
            alterTableAdd |
            alterTableDrop // Combined DROP COLUMN and DROP CONSTRAINT parser
        )
    ).map { case (name, action) => OracleAlterTable(name, action) }
  )

  def alterTableModify[$ : P]: P[AlterTableAction] = P(
    keyword("MODIFY") ~/
      (
        // MODIFY(column type, ...) - with parentheses
        ("(" ~/
          columnModification.rep(sep = comma, min = 1) ~
          ")").map(mods => AlterTableModify(mods)) |
          // MODIFY column type - without parentheses (single or multiple columns)
          (columnModification ~
            (comma ~/ keyword("MODIFY") ~/ columnModification).rep).map { case (first, rest) =>
            AlterTableModify(Seq(first) ++ rest)
          }
      )
  )

  def alterTableRenameColumn[$ : P]: P[AlterTableAction] = P(
    keyword("RENAME") ~/
      keyword("COLUMN") ~/
      identifier ~
      keyword("TO") ~/
      identifier
  ).map { case (oldName, newName) => AlterTableRenameColumn(oldName, newName) }

  def alterTableAdd[$ : P]: P[AlterTableAction] = P(
    keyword("ADD") ~/
      (
        // ADD CONSTRAINT constraint_name UNIQUE/PRIMARY KEY/FOREIGN KEY (...)
        (keyword("CONSTRAINT") ~/
          identifier ~
          tableConstraint).map { case (constraintName, constraint) =>
          AlterTableAddConstraint(constraintName, constraint)
        } |
          // ADD(column, ...) - with parentheses
          ("(" ~/
            tableField.rep(sep = comma, min = 1) ~
            ")").map { fields =>
            val columns = fields.collect { case Right(colDef) => colDef }
            val annotations = fields.collect { case Left(ann) => ann }
            AlterTableAdd(columns.toSeq, annotations.toSeq)
          } |
          // ADD column - without parentheses (single or multiple columns)
          (tableField ~
            (keyword("ADD") ~/ tableField).rep).map { case (first, rest) =>
            val allFields = Seq(first) ++ rest
            val columns = allFields.collect { case Right(colDef) => colDef }
            val annotations = allFields.collect { case Left(ann) => ann }
            AlterTableAdd(columns, annotations)
          }
      )
  )

  def tableConstraint[$ : P]: P[TableConstraint] = P(
    // UNIQUE (col1, col2, ...)
    (keyword("UNIQUE") ~/
      "(" ~/ identifier.rep(sep = comma, min = 1).map(_.toSeq) ~ ")").map { columns =>
      UniqueConstraint(columns)
    } |
      // PRIMARY KEY (col1, col2, ...)
      (keyword("PRIMARY") ~/
        keyword("KEY") ~/
        "(" ~/ identifier.rep(sep = comma, min = 1).map(_.toSeq) ~ ")").map { columns =>
        PrimaryKeyConstraint(columns)
      } |
      // FOREIGN KEY (col1, ...) REFERENCES table(col1, ...)
      (keyword("FOREIGN") ~/
        keyword("KEY") ~/
        "(" ~/ identifier.rep(sep = comma, min = 1).map(_.toSeq) ~ ")" ~
        keyword("REFERENCES") ~/
        oracleQualifiedTableName ~
        "(" ~/ identifier.rep(sep = comma, min = 1).map(_.toSeq) ~ ")" ~
        (keyword("ON") ~/ keyword("DELETE") ~/ referentialAction).? ~
        (keyword("ON") ~/ keyword("UPDATE") ~/ referentialAction).?).map {
        case (columns, refTable, refColumns, onDelete, onUpdate) =>
          ForeignKeyConstraint(columns, refTable, refColumns, onDelete, onUpdate)
      }
  )

  def referentialAction[$ : P]: P[ReferentialAction] = P(
    (keyword("NO") ~/ keyword("ACTION")).map(_ => ReferentialAction.NoAction) |
      keyword("RESTRICT").map(_ => ReferentialAction.Restrict) |
      keyword("CASCADE").map(_ => ReferentialAction.Cascade) |
      (keyword("SET") ~/ keyword("NULL")).map(_ => ReferentialAction.SetNull) |
      (keyword("SET") ~/ keyword("DEFAULT")).map(_ => ReferentialAction.SetDefault)
  )

  def alterTableDrop[$ : P]: P[AlterTableAction] = P(
    keyword("DROP") ~/
      (
        // DROP (col1, col2, ...) - multiple columns in parentheses
        ("(" ~/
          identifier.rep(sep = comma, min = 1).map(_.toSeq) ~
          ")").map(cols => AlterTableDropColumns(cols)) |
          // DROP COLUMN col_name - single column
          keyword("COLUMN") ~/ identifier.map { colName =>
            AlterTableDropColumn(colName)
          } |
          // DROP CONSTRAINT constraint_name
          keyword("CONSTRAINT") ~/ identifier.map { constraintName =>
            AlterTableDropConstraint(constraintName)
          }
      )
  )

  def alterTableDropColumn[$ : P]: P[AlterTableAction] = P(
    keyword("DROP") ~/
      keyword("COLUMN") ~/
      identifier
  ).map(colName => AlterTableDropColumn(colName))

  def alterTableDropConstraint[$ : P]: P[AlterTableAction] = P(
    keyword("DROP") ~/
      keyword("CONSTRAINT") ~/
      identifier
  ).map(constraintName => AlterTableDropConstraint(constraintName))

  def columnModification[$ : P]: P[ColumnModification] = P(
    (
      identifier ~/
        // Type is optional - MODIFY column DEFAULT/NOT NULL is valid without type
        // Use lookahead to check if this looks like a type (not a keyword like DEFAULT/NOT)
        (
          // Only parse type if NOT followed by annotation keywords
          !(&(keyword("DEFAULT") | keyword("NOT") | keyword("PRIMARY"))) ~
            (oracleTypeName ~
              typeParameters.?).map { case (typeName, paramsOpt) =>
              Some(Name(typeName.name + paramsOpt.getOrElse("")))
            }
        ).? ~
        oracleColumnAnnotation.rep
    ).map { case (name: Name, typeOpt, annotations) =>
      ColumnModification(name, typeOpt.flatten, annotations)
    }
  )

  override def dropTableOrView[$ : P]: P[OracleStatement] = P(
    (
      keyword("DROP") ~
        keyword("TABLE", "VIEW").!.map(_.toUpperCase) ~/
        ifExists ~
        oracleQualifiedTableName ~
        (keyword("CASCADE") ~/ StringInIgnoreCase("CONSTRAINTS")).!.?.map(_.isDefined) ~
        keyword("PURGE").!.?.map(_.isDefined)
    ).map {
      case ("TABLE", ifExists, name, cascadeConstraints, purge) =>
        OracleDropTable(name, ifExists, cascadeConstraints, purge)
      case ("VIEW", ifExists, name, _, _) =>
        OracleDropView(name, ifExists)
      case (_, _, _, _, _) =>
        throw new Exception("Internal Error")
    }
  )

  def dropSynonym[$ : P]: P[OracleStatement] = P(
    (
      keyword("DROP") ~
        keyword("SYNONYM") ~/
        ifExists ~
        oracleQualifiedTableName
    ).map { case (ifExists, name) => OracleDropSynonym(name, ifExists) }
  )

  def rename[$ : P]: P[OracleStatement] = P(
    (
      keyword("RENAME") ~/
        oracleQualifiedTableName ~
        keyword("TO") ~/
        oracleQualifiedTableName
    ).map { case (oldName, newName) => OracleRename(oldName, newName) }
  )

  // Parser for fully qualified column names (schema.table.column)
  def oracleQualifiedColumnName[$ : P]: P[Name] = P(
    (identifier ~ ("." ~/ identifier).rep(min = 1, max = 2)).map { case (first, rest) =>
      val parts = Seq(first) ++ rest.toSeq
      val nameStr = parts.map(_.name).mkString(".")
      val quoted = parts.exists(_.quoted)
      Name(nameStr, quoted)
    }
  )

  def comment[$ : P]: P[Comment] = P(
    (
      keyword("COMMENT") ~/
        keyword("ON") ~/
        keyword("COLUMN") ~/
        oracleQualifiedColumnName ~
        keyword("IS") ~/
        quotedString
    ).map { case (columnName, commentText) => Comment(columnName, commentText) }
  )

  // PL/SQL recursive parser combinators for blocks and loops
  // Uses FastParse's recursive descent to naturally handle nesting

  // Parse a PL/SQL label: <<label>>
  def plsqlLabel[$ : P]: P[String] = P("<<" ~/ identifier.map(_.name) ~ ">>")

  // Parse DECLARE section (captured as string for now)
  def plsqlDeclareSection[$ : P]: P[String] = P(
    keyword("DECLARE") ~/
      // Parse until BEGIN, handling nested structures
      (!(&(keyword("BEGIN")) ~/ !CharIn("a-zA-Z0-9_")) ~ AnyChar).rep.!
  )

  // Parse EXCEPTION handlers (captured as string for now)
  def plsqlExceptionSection[$ : P]: P[String] = P(
    keyword("EXCEPTION") ~/
      // Parse until END, handling nested structures
      (!(&(keyword("END")) ~/ !CharIn("a-zA-Z0-9_")) ~ AnyChar).rep.!
  )

  // Parse FOR loop parameter: variable IN (SELECT ...) or variable IN 1..10
  def plsqlForLoopParam[$ : P]: P[(Name, Either[SelectBody, String])] = P(
    identifier ~
      keyword("IN") ~/
      (
        // Cursor query: FOR rec IN (SELECT ...) - try this first!
        ("(" ~/ select ~ ")").map(query => Left(query)) |
          // Numeric range: FOR i IN 1..10 or FOR i IN REVERSE 1..10
          (keyword("REVERSE").!.? ~
            expression ~ ".." ~ expression).map { case (reverse, lower, upper) =>
            Right(s"${if (reverse.isDefined) "REVERSE " else ""}$lower..$upper")
          } |
          // Cursor name: FOR rec IN cursor_name
          identifier.map(cursorName => Right(cursorName.name))
      )
  )

  // Parse WHILE loop condition
  def plsqlWhileCondition[$ : P]: P[String] = P(
    // Parse expression until LOOP keyword
    (!(&(keyword("LOOP")) ~/ !CharIn("a-zA-Z0-9_")) ~ AnyChar).rep.!
  )

  // Parse a single PL/SQL statement - defined first but references blocks/loops below
  // FastParse handles forward references through lazy evaluation
  def plsqlStatement[$ : P]: P[String] = P(
    // Try to parse structured constructs first (they're recursive)
    plsqlBlock.map(_.toString) |
      plsqlLoop.map(_.toString) |
      // Otherwise, parse a simple statement until semicolon or end of block
      (!(keyword("END") | keyword("EXCEPTION") | keyword("ELSE") | keyword("ELSIF")) ~
        !(&(keyword("END") ~ keyword("LOOP")) ~/ !CharIn("a-zA-Z0-9_")) ~
        AnyChar).rep(1).! ~ (";".? | Pass)
  )

  // Parse sequence of statements (recursive)
  def plsqlStatementSequence[$ : P]: P[String] = P(plsqlStatement.rep(sep = Pass).map(_.mkString(" ")))

  // Parse PL/SQL loop (FOR or WHILE) - simplified for testing
  def plsqlLoop[$ : P]: P[OracleStatement] = P(
    // FOR loop
    (keyword("FOR") ~/
      plsqlForLoopParam ~
      keyword("LOOP") ~/
      plsqlStatementSequence ~
      keyword("END") ~/
      keyword("LOOP") ~
      ";".?).map { case (varName, queryOrRange, statements) =>
      queryOrRange match {
        case Left(query) =>
          PlSqlForLoop(varName, query, statements.trim)
        case Right(rangeStr) =>
          PlSqlForLoop(
            varName,
            SelectBody(
              distinct = false,
              target = Seq(),
              from = Seq(),
              where = None,
              groupBy = None,
              having = None,
              orderBy = Seq(),
              limit = None,
              offset = None,
              union = None
            ),
            s"$rangeStr\n$statements".trim
          )
      }
    } |
      // WHILE loop
      (keyword("WHILE") ~/
        plsqlWhileCondition ~
        keyword("LOOP") ~/
        plsqlStatementSequence ~
        keyword("END") ~/
        keyword("LOOP") ~
        ";".?).map { case (condition, statements) =>
        PlSqlWhileLoop(condition.trim, statements.trim)
      }
  )

  // Parse PL/SQL anonymous block (recursive)
  def plsqlBlock[$ : P]: P[OracleStatement] = P(
    // Optional DECLARE section
    (keyword("DECLARE") ~/ plsqlDeclareSection).? ~
      keyword("BEGIN") ~/
      // Parse sequence of statements (can contain nested blocks/loops)
      plsqlStatementSequence ~
      // Optional EXCEPTION section
      (keyword("EXCEPTION") ~/ plsqlExceptionSection).? ~
      keyword("END") ~
      (plsqlLabel | Pass).? ~
      ";".?
  ).map { case (declareOpt: Option[String], statements: String, exceptionOpt: Option[String], _) =>
    val declareStr = declareOpt.getOrElse("")
    val exceptionStr = exceptionOpt.getOrElse("")
    val content = if (declareStr.nonEmpty) s"$declareStr\n$statements" else statements
    val finalContent = if (exceptionStr.nonEmpty) s"$content\n$exceptionStr" else content
    PlSqlBlock(finalContent.trim)
  }

  // Main PL/SQL block parser entry point - try plsqlBlock first
  def plSqlBlock[$ : P]: P[OracleStatement] = P(plsqlBlock | plsqlLoop)

  def createSynonym[$ : P]: P[OracleStatement] = P(
    (
      keyword("CREATE") ~
        orReplace ~
        keyword("SYNONYM") ~/
        oracleQualifiedTableName ~
        keyword("FOR") ~/
        oracleQualifiedTableName
    ).map { case (orReplace, name, target) =>
      OracleCreateSynonym(name, orReplace, target)
    }
  )

  def createIndex[$ : P]: P[OracleStatement] = P(
    (
      keyword("CREATE") ~
        keyword("UNIQUE").!.?.map(_.isDefined) ~
        keyword("INDEX") ~/
        oracleQualifiedTableName ~
        keyword("ON") ~/
        oracleQualifiedTableName ~
        "(" ~/ identifier.rep(sep = comma, min = 1).map(_.toSeq) ~ ")"
    ).map { case (unique, indexName, tableName, columns) =>
      OracleCreateIndex(indexName, tableName, columns, unique)
    }
  )

  override def explainStatement[$ : P]: P[OracleStatement] = P(keyword("EXPLAIN") ~ select.map(s => OracleExplain(s)))

  override def createView[$ : P]: P[Stmt] = P(
    (
      keyword("CREATE") ~
        orReplace ~
        // MATERIALIZED and TEMPORARY are mutually exclusive with FORCE/EDITIONABLE
        (keyword("MATERIALIZED").map(_ => (true, false, false, false)) |
          keyword("TEMPORARY").map(_ => (false, true, false, false)) |
          // Regular view: parse optional FORCE and EDITIONABLE keywords
          (
            // Both keywords: FORCE EDITIONABLE
            (StringInIgnoreCase("FORCE") ~ StringInIgnoreCase("EDITIONABLE")).map { _ =>
              (false, false, true, true)
            } |
              // Both keywords: EDITIONABLE FORCE
              (StringInIgnoreCase("EDITIONABLE") ~ StringInIgnoreCase("FORCE")).map { _ =>
                (false, false, true, true)
              } |
              // Just FORCE
              StringInIgnoreCase("FORCE").map(_ => (false, false, true, false)) |
              // Just EDITIONABLE
              StringInIgnoreCase("EDITIONABLE").map(_ => (false, false, false, true)) |
              // No keywords
              Pass.map(_ => (false, false, false, false))
          )) ~
        keyword("VIEW") ~/
        oracleQualifiedTableName ~
        // Column list only allowed for regular views (not materialized/temporary)
        ("(" ~/
          identifier.rep(sep = comma, min = 1).map(_.toSeq) ~
          ")").? ~
        keyword("AS") ~/
        select
    ).map { case (orReplace, (materialized, temporary, force, editionable), name, columnList, query) =>
      OracleCreateView(name, orReplace, query, materialized, temporary, force, editionable, columnList)
    }
  )

  override def statementTerminatorParser[$ : P]: P[Unit] = P(";") | P("/" ~ !"*")
  // Override terminatedStatement to handle unparseable statements
  // NOTE: Does NOT handle End here - that would cause infinite loops with .rep
  override def terminatedStatement[$ : P]: P[Stmt] = P(
    (statement ~ (statementTerminatorParser | (";".! ~ Pass) | Pass)).map { case (stmt, _) => stmt } |
      unparseableStatement // Fallback for parse errors (requires at least one char)
  )

  // Override allStatements to handle statements without terminators at the end
  override def allStatements[$ : P]: P[Seq[Stmt]] = P(Start ~ terminatedStatement.rep ~ (End | Pass)).map(_.toSeq)

  // Parser for empty statements (whitespace/comments only)
  // This matches when only whitespace/comments remain before the terminator
  // Handles both cases: comments ending with ; or / on next line
  def emptyStatement[$ : P]: P[EmptyStatement] = P(
    Pass ~ // Consume any remaining whitespace/comments (including ; at end of comment lines)
      (End.map(_ => EmptyStatement()) // End of input
        | &(CharIn("/;")).map(_ => EmptyStatement())) // Lookahead for terminator without consuming it
  )

  override def statement[$ : P]: P[Stmt] = P(
    Pass ~ // This trims off leading whitespace (including comments)
      ( // Check for WITH clause first (top-level WITH ... SELECT)
        (&(keyword("WITH")) ~/ withSelect.map { case (ctes, body) => OracleSelect(body, ctes) }) |
          // Check for PL/SQL blocks before other statements
          plSqlBlock |
          // All other statements (including SELECT without WITH, which will be handled by base class)
          (parenthesizedSelect.map(body => OracleSelect(body, Seq.empty))
            | update
            | delete
            | insert
            | createStatement // Delegates to subclass
            | (&(keyword("ALTER")) ~/ (alterTable | alterView))
            | dropTableOrView
            | explainStatement
            | dialectSpecificStatement // Hook for dialect extensions
            | emptyStatement // Accept empty statements (whitespace/comments only) when only terminator remains
          )
      )
  )

  override def createStatement[$ : P]: P[Stmt] = P(
    &(keyword("CREATE")) ~/ (createView | oracleCreateTable | createSynonym | createIndex)
  )

  def oracleCreateTable[$ : P]: P[Stmt] = P(
    (
      keyword("CREATE") ~
        orReplace ~
        keyword("TABLE") ~/
        oracleQualifiedTableName ~
        (
          (keyword("AS") ~/ select).map(Left(_))
            | ("(" ~/
              tableField.rep(sep = comma) ~
              ")").map(Right(_))
        ) ~
        oracleTableClauses.?
    ).map {
      case (orReplace, table, Left(query), clauses) =>
        val clausesOpt = clauses.getOrElse(OracleTableClauses())
        OracleCreateTable(
          table,
          orReplace,
          Left(query),
          Seq.empty,
          clausesOpt.tablespace,
          clausesOpt.pctUsed,
          clausesOpt.pctFree,
          clausesOpt.storage,
          clausesOpt.logging,
          clausesOpt.compress,
          clausesOpt.cache,
          clausesOpt.parallel,
          clausesOpt.monitoring
        )
      case (orReplace, table, Right(fields), clauses) =>
        val columns = fields.collect { case Right(r) => r }
        val annotations = fields.collect { case Left(l) => l }
        val clausesOpt = clauses.getOrElse(OracleTableClauses())
        OracleCreateTable(
          table,
          orReplace,
          Right(columns),
          annotations,
          clausesOpt.tablespace,
          clausesOpt.pctUsed,
          clausesOpt.pctFree,
          clausesOpt.storage,
          clausesOpt.logging,
          clausesOpt.compress,
          clausesOpt.cache,
          clausesOpt.parallel,
          clausesOpt.monitoring
        )
    }
  )

  case class OracleTableClauses(
    tablespace: Option[String] = None,
    pctUsed: Option[Int] = None,
    pctFree: Option[Int] = None,
    storage: Option[StorageClause] = None,
    logging: Option[scala.Boolean] = None,
    compress: Option[scala.Boolean] = None,
    cache: Option[scala.Boolean] = None,
    parallel: Option[scala.Boolean] = None,
    monitoring: Option[scala.Boolean] = None
  )

  def oracleTableClauses[$ : P]: P[OracleTableClauses] = P(
    tablespaceClause.? ~
      pctUsedClause.? ~
      pctFreeClause.? ~
      initransClause.? ~/ // Make optional and discard
      maxtransClause.? ~/ // Make optional and discard
      storageClause.? ~
      loggingClause.? ~
      compressClause.? ~
      cacheClause.? ~
      parallelClause.? ~
      monitoringClause.?
  ).map { case (ts, pu, pf, st, log, comp, cch, par, mon) =>
    OracleTableClauses(ts, pu, pf, st, log, comp, cch, par, mon)
  }

  def tablespaceClause[$ : P]: P[String] = P(StringInIgnoreCase("TABLESPACE") ~/ identifier.map(_.name))

  def pctUsedClause[$ : P]: P[Int] = P(StringInIgnoreCase("PCTUSED") ~/ integer.map(_.toInt))

  def pctFreeClause[$ : P]: P[Int] = P(StringInIgnoreCase("PCTFREE") ~/ integer.map(_.toInt))

  def initransClause[$ : P]: P[Unit] = P(StringInIgnoreCase("INITRANS") ~/ integer.map(_ => ()))

  def maxtransClause[$ : P]: P[Unit] = P(StringInIgnoreCase("MAXTRANS") ~/ integer.map(_ => ()))

  def storageClause[$ : P]: P[StorageClause] = P(
    StringInIgnoreCase("STORAGE") ~/ "(" ~/
      storageOptions ~
      ")"
  )

  def storageOptions[$ : P]: P[StorageClause] = P(
    (storageInitial.?
      ~ storageNext.?
      ~ storageMinExtents.?
      ~ storageMaxExtents.?
      ~ storagePctIncrease.?
      ~ storageBufferPool.?).map { case (init, next, min, max, pct, buf) =>
      StorageClause(init, next, min, max, pct, buf)
    }
  )

  def storageInitial[$ : P]: P[String] = P(StringInIgnoreCase("INITIAL") ~/ (integer.! ~ CharIn("KMG").!.?).map {
    case (n, Some(u)) => s"$n$u"
    case (n, None) => n
  })

  def storageNext[$ : P]: P[String] = P(StringInIgnoreCase("NEXT") ~/ (integer.! ~ CharIn("KMG").!.?).map {
    case (n, Some(u)) => s"$n$u"
    case (n, None) => n
  })

  def storageMinExtents[$ : P]: P[Int] = P(StringInIgnoreCase("MINEXTENTS") ~/ integer.map(_.toInt))

  def storageMaxExtents[$ : P]: P[String] = P(
    StringInIgnoreCase("MAXEXTENTS") ~/ (
      StringInIgnoreCase("UNLIMITED").!.map(_ => "UNLIMITED")
        | integer.!.map(_.toString)
    )
  )

  def storagePctIncrease[$ : P]: P[Int] = P(StringInIgnoreCase("PCTINCREASE") ~/ integer.map(_.toInt))

  def storageBufferPool[$ : P]: P[String] = P(
    StringInIgnoreCase("BUFFER_POOL") ~/ (
      StringInIgnoreCase("DEFAULT").!.map(_ => "DEFAULT")
        | identifier.map(_.name)
    )
  )

  def loggingClause[$ : P]: P[scala.Boolean] = P(
    StringInIgnoreCase("LOGGING").map(_ => true) | StringInIgnoreCase("NOLOGGING").map(_ => false)
  )

  def compressClause[$ : P]: P[scala.Boolean] = P(
    StringInIgnoreCase("COMPRESS").map(_ => true) | StringInIgnoreCase("NOCOMPRESS").map(_ => false)
  )

  def cacheClause[$ : P]: P[scala.Boolean] = P(
    StringInIgnoreCase("CACHE").map(_ => true) | StringInIgnoreCase("NOCACHE").map(_ => false)
  )

  def parallelClause[$ : P]: P[scala.Boolean] = P(
    StringInIgnoreCase("PARALLEL").map(_ => true) | StringInIgnoreCase("NOPARALLEL").map(_ => false)
  )

  def monitoringClause[$ : P]: P[scala.Boolean] = P(
    StringInIgnoreCase("MONITORING").map(_ => true) | StringInIgnoreCase("NOMONITORING").map(_ => false)
  )

  def dataType[$ : P]: P[DataType] = P(
    // Oracle-specific types first
    (keyword("VARCHAR2") ~ "(" ~ integer.map(_.toInt) ~
      (keyword("BYTE") | keyword("CHAR")).!.?.map(_.map(_.toUpperCase)) ~ ")")
      .map { case (len, bc) => VarChar2(len, bc) }
      | (keyword("NVARCHAR2") ~ "(" ~ integer.map(_.toInt) ~ ")")
        .map(len => NVarChar2(len))
      | (keyword("NUMBER") ~
        ("(" ~ integer.map(_.toInt) ~ ("," ~ integer.map(_.toInt)).? ~ ")").?)
        .map {
          case Some((p, Some(s))) => Number(Some(p), Some(s))
          case Some((p, None)) => Number(Some(p), None)
          case None => Number(None, None)
        }
      | keyword("CLOB").map(_ => Clob())
      | keyword("BLOB").map(_ => Blob())
      | (keyword("RAW") ~ "(" ~ integer.map(_.toInt) ~ ")")
        .map(len => Raw(len))
      // Fall back to common types
      | (keyword("VARCHAR") ~ "(" ~ integer.map(_.toInt) ~ ")").map(VarChar(_))
      | (keyword("CHAR") ~ "(" ~ integer.map(_.toInt) ~
        (keyword("BYTE") | keyword("CHAR")).!.?.map(_ => ()) ~ ")").map(Char(_))
      | keyword("INTEGER").map(_ => Integer())
      | keyword("INT").map(_ => Integer())
      | keyword("BIGINT").map(_ => BigInt())
      | (keyword("DECIMAL") ~ "(" ~ integer.map(_.toInt) ~ "," ~ integer.map(_.toInt) ~ ")")
        .map { case (p, s) => Decimal(p, s) }
      | keyword("FLOAT").map(_ => Float())
      | keyword("DOUBLE").map(_ => Double())
      | keyword("DATE").map(_ => Date())
      | (keyword("TIMESTAMP") ~ ("(" ~ integer.map(_.toInt) ~ ")").?)
        .map(p => Timestamp(p))
      | keyword("BOOLEAN").map(_ => Boolean())
      | keyword("TEXT").map(_ => Text())
  )
}

object OracleSQL extends SQLBaseObject {
  type Stmt = Statement
  import fastparse._
  import fastparse.Parsed
  private val latest = new OracleSQL()

  def name: String = "oracle"

  protected def statementTerminator: String = "/"

  override protected def statementTerminatorChars: Set[scala.Char] = Set(';', '/')

  def apply(input: String): Parsed[Stmt] =
    parse(input, latest.terminatedStatement(_))

  def apply(input: Reader): StreamParser[Stmt] =
    new StreamParser[Stmt](parse(_: Iterator[String], latest.terminatedStatement(_), verboseFailures = true), input)
}

// Version-specific subclasses
class Oracle11gSQL extends OracleSQL {
  override def supportsReturning = false // 11g limitation
}

object Oracle11g {
  type Stmt = Statement
  import fastparse._
  import fastparse.Parsed
  private val instance = new Oracle11gSQL()

  def apply(input: String): Parsed[Stmt] =
    parse(input, instance.terminatedStatement(_))

  def apply(input: Reader): StreamParser[Stmt] =
    new StreamParser[Stmt](parse(_: Iterator[String], instance.terminatedStatement(_), verboseFailures = true), input)
}
