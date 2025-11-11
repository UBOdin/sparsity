package sparsity.common.codec

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import io.circe.{Json => CirceJson}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{deriveConfiguredDecoder, deriveConfiguredEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import cats.syntax.traverse._
import sparsity.common.{
  Array => SArray,
  BigInt => SBigInt,
  BigSerial => SBigSerial,
  Blob => SBlob,
  Boolean => SBoolean,
  Char => SChar,
  Cidr => SCidr,
  Clob => SClob,
  DataType,
  Date => SDate,
  Decimal => SDecimal,
  Double => SDouble,
  Float => SFloat,
  Inet => SInet,
  Integer => SInteger,
  Json => SJson,
  Jsonb => SJsonb,
  MacAddr => SMacAddr,
  NVarChar2 => SNVarChar2,
  Name,
  Number => SNumber,
  Raw => SRaw,
  Serial => SSerial,
  Text => SText,
  Timestamp => STimestamp,
  Uuid => SUuid,
  VarChar => SVarChar,
  VarChar2 => SVarChar2
}
import sparsity.common.expression._
import sparsity.common.statement._
import sparsity.common.select._
import sparsity.common.alter._
import sparsity.oracle._

/** Circe codecs for all parser result datatypes */
object CirceCodecs {
  // Configuration for sealed trait/class hierarchies with "type" discriminator
  implicit val config: Configuration = Configuration.default.withDiscriminator("type")

  // Name codec
  implicit val nameEncoder: Encoder[Name] = deriveEncoder[Name]
  implicit val nameDecoder: Decoder[Name] = deriveDecoder[Name]

  // Function codec (needs explicit codec due to generic derivation issues)
  implicit val functionEncoder: Encoder[Function] = deriveEncoder[Function]
  implicit val functionDecoder: Decoder[Function] = deriveDecoder[Function]

  // DataType codecs
  implicit val dataTypeEncoder: Encoder[DataType] = Encoder.instance {
    case v: SVarChar =>
      CirceJson.obj("type" -> CirceJson.fromString("VarChar"), "length" -> CirceJson.fromInt(v.length))
    case c: SChar => CirceJson.obj("type" -> CirceJson.fromString("Char"), "length" -> CirceJson.fromInt(c.length))
    case SInteger() => CirceJson.obj("type" -> CirceJson.fromString("Integer"))
    case SBigInt() => CirceJson.obj("type" -> CirceJson.fromString("BigInt"))
    case d: SDecimal =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Decimal"),
        "precision" -> CirceJson.fromInt(d.precision),
        "scale" -> CirceJson.fromInt(d.scale)
      )
    case SFloat() => CirceJson.obj("type" -> CirceJson.fromString("Float"))
    case SDouble() => CirceJson.obj("type" -> CirceJson.fromString("Double"))
    case SDate() => CirceJson.obj("type" -> CirceJson.fromString("Date"))
    case t: STimestamp => CirceJson.obj("type" -> CirceJson.fromString("Timestamp"), "precision" -> t.precision.asJson)
    case SBoolean() => CirceJson.obj("type" -> CirceJson.fromString("Boolean"))
    case SText() => CirceJson.obj("type" -> CirceJson.fromString("Text"))
    case v: SVarChar2 =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("VarChar2"),
        "length" -> CirceJson.fromInt(v.length),
        "byteOrChar" -> v.byteOrChar.asJson
      )
    case n: SNumber =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Number"),
        "precision" -> n.precision.asJson,
        "scale" -> n.scale.asJson
      )
    case SClob() => CirceJson.obj("type" -> CirceJson.fromString("Clob"))
    case SBlob() => CirceJson.obj("type" -> CirceJson.fromString("Blob"))
    case n: SNVarChar2 =>
      CirceJson.obj("type" -> CirceJson.fromString("NVarChar2"), "length" -> CirceJson.fromInt(n.length))
    case r: SRaw => CirceJson.obj("type" -> CirceJson.fromString("Raw"), "length" -> CirceJson.fromInt(r.length))
    case SSerial() => CirceJson.obj("type" -> CirceJson.fromString("Serial"))
    case SBigSerial() => CirceJson.obj("type" -> CirceJson.fromString("BigSerial"))
    case SJson() => CirceJson.obj("type" -> CirceJson.fromString("Json"))
    case SJsonb() => CirceJson.obj("type" -> CirceJson.fromString("Jsonb"))
    case SUuid() => CirceJson.obj("type" -> CirceJson.fromString("Uuid"))
    case SInet() => CirceJson.obj("type" -> CirceJson.fromString("Inet"))
    case SCidr() => CirceJson.obj("type" -> CirceJson.fromString("Cidr"))
    case SMacAddr() => CirceJson.obj("type" -> CirceJson.fromString("MacAddr"))
    case a: SArray =>
      CirceJson.obj("type" -> CirceJson.fromString("Array"), "elementType" -> dataTypeEncoder(a.elementType))
  }

  implicit val dataTypeDecoder: Decoder[DataType] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "VarChar" => c.get[Int]("length").map(SVarChar.apply)
      case "Char" => c.get[Int]("length").map(SChar.apply)
      case "Integer" => Right(SInteger())
      case "BigInt" => Right(SBigInt())
      case "Decimal" =>
        for {
          p <- c.get[Int]("precision")
          s <- c.get[Int]("scale")
        } yield SDecimal(p, s)
      case "Float" => Right(SFloat())
      case "Double" => Right(SDouble())
      case "Date" => Right(SDate())
      case "Timestamp" => c.get[Option[Int]]("precision").map(STimestamp.apply)
      case "Boolean" => Right(SBoolean())
      case "Text" => Right(SText())
      case "VarChar2" =>
        for {
          l <- c.get[Int]("length")
          boc <- c.get[Option[String]]("byteOrChar")
        } yield SVarChar2(l, boc)
      case "Number" =>
        for {
          p <- c.get[Option[Int]]("precision")
          s <- c.get[Option[Int]]("scale")
        } yield SNumber(p, s)
      case "Clob" => Right(SClob())
      case "Blob" => Right(SBlob())
      case "NVarChar2" => c.get[Int]("length").map(SNVarChar2.apply)
      case "Raw" => c.get[Int]("length").map(SRaw.apply)
      case "Serial" => Right(SSerial())
      case "BigSerial" => Right(SBigSerial())
      case "Json" => Right(SJson())
      case "Jsonb" => Right(SJsonb())
      case "Uuid" => Right(SUuid())
      case "Inet" => Right(SInet())
      case "Cidr" => Right(SCidr())
      case "MacAddr" => Right(SMacAddr())
      case "Array" => c.get[DataType]("elementType")(dataTypeDecoder).map(SArray.apply)
      case other => Left(DecodingFailure(s"Unknown DataType: $other", c.history))
    }
  }

  // Enum codecs
  implicit val arithmeticOpEncoder: Encoder[Arithmetic.Op] = Encoder.encodeString.contramap(_.toString)
  implicit val arithmeticOpDecoder: Decoder[Arithmetic.Op] = Decoder.decodeString.emap { s =>
    try Right(Arithmetic.withName(s).asInstanceOf[Arithmetic.Op])
    catch { case _: NoSuchElementException => Left(s"Unknown Arithmetic.Op: $s") }
  }

  implicit val comparisonOpEncoder: Encoder[Comparison.Op] = Encoder.encodeString.contramap(_.toString)
  implicit val comparisonOpDecoder: Decoder[Comparison.Op] = Decoder.decodeString.emap { s =>
    try Right(Comparison.withName(s).asInstanceOf[Comparison.Op])
    catch { case _: NoSuchElementException => Left(s"Unknown Comparison.Op: $s") }
  }

  implicit val joinTypeEncoder: Encoder[Join.Type] = Encoder.encodeString.contramap(_.toString)
  implicit val joinTypeDecoder: Decoder[Join.Type] = Decoder.decodeString.emap { s =>
    try Right(Join.withName(s).asInstanceOf[Join.Type])
    catch { case _: NoSuchElementException => Left(s"Unknown Join.Type: $s") }
  }

  implicit val unionTypeEncoder: Encoder[Union.Type] = Encoder.encodeString.contramap(_.toString)
  implicit val unionTypeDecoder: Decoder[Union.Type] = Decoder.decodeString.emap { s =>
    try Right(Union.withName(s).asInstanceOf[Union.Type])
    catch { case _: NoSuchElementException => Left(s"Unknown Union.Type: $s") }
  }

  // Expression codecs (recursive)
  implicit val expressionEncoder: Encoder[Expression] = Encoder.instance {
    case lp: LongPrimitive =>
      CirceJson.obj("type" -> CirceJson.fromString("LongPrimitive"), "value" -> CirceJson.fromLong(lp.v))
    case dp: DoublePrimitive =>
      CirceJson.obj("type" -> CirceJson.fromString("DoublePrimitive"), "value" -> CirceJson.fromDoubleOrNull(dp.v))
    case sp: StringPrimitive =>
      CirceJson.obj("type" -> CirceJson.fromString("StringPrimitive"), "value" -> CirceJson.fromString(sp.v))
    case bp: BooleanPrimitive =>
      CirceJson.obj("type" -> CirceJson.fromString("BooleanPrimitive"), "value" -> CirceJson.fromBoolean(bp.v))
    case NullPrimitive() => CirceJson.obj("type" -> CirceJson.fromString("NullPrimitive"))
    case c: Column =>
      CirceJson.obj("type" -> CirceJson.fromString("Column"), "column" -> c.column.asJson, "table" -> c.table.asJson)
    case a: Arithmetic =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Arithmetic"),
        "lhs" -> a.lhs.asJson,
        "op" -> a.op.asJson,
        "rhs" -> a.rhs.asJson
      )
    case c: Comparison =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Comparison"),
        "lhs" -> c.lhs.asJson,
        "op" -> c.op.asJson,
        "rhs" -> c.rhs.asJson
      )
    case f: Function =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Function"),
        "name" -> f.name.asJson,
        "params" -> f.params.asJson,
        "distinct" -> CirceJson.fromBoolean(f.distinct)
      )
    case wf: WindowFunction =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("WindowFunction"),
        "function" -> wf.function.asJson,
        "partitionBy" -> wf.partitionBy.asJson,
        "orderBy" -> wf.orderBy.asJson
      )
    case JDBCVar() => CirceJson.obj("type" -> CirceJson.fromString("JDBCVar"))
    case sq: Subquery => CirceJson.obj("type" -> CirceJson.fromString("Subquery"), "query" -> sq.query.asJson)
    case cwe: CaseWhenElse =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("CaseWhenElse"),
        "target" -> cwe.target.asJson,
        "cases" -> cwe.cases.map { case (e1, e2) => CirceJson.obj("when" -> e1.asJson, "then" -> e2.asJson) }.asJson,
        "otherwise" -> cwe.otherwise.asJson
      )
    case in: IsNull => CirceJson.obj("type" -> CirceJson.fromString("IsNull"), "target" -> in.target.asJson)
    case n: Not => CirceJson.obj("type" -> CirceJson.fromString("Not"), "target" -> n.target.asJson)
    case cast: Cast =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Cast"),
        "expression" -> cast.expression.asJson,
        "typeName" -> cast.t.asJson
      )
    case ie: InExpression =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("InExpression"),
        "expression" -> ie.expression.asJson,
        "source" -> (ie.source match {
          case Left(exprs) => CirceJson.obj("type" -> CirceJson.fromString("Expressions"), "values" -> exprs.asJson)
          case Right(query) => CirceJson.obj("type" -> CirceJson.fromString("SelectBody"), "query" -> query.asJson)
        })
      )
    case _: NegatableExpression => // Handled by IsNull and InExpression cases above
      throw new IllegalStateException("Unhandled NegatableExpression subtype")
  }

  implicit val expressionDecoder: Decoder[Expression] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "LongPrimitive" => c.get[Long]("value").map(LongPrimitive.apply)
      case "DoublePrimitive" => c.get[Double]("value").map(DoublePrimitive.apply)
      case "StringPrimitive" => c.get[String]("value").map(StringPrimitive.apply)
      case "BooleanPrimitive" => c.get[scala.Boolean]("value").map(BooleanPrimitive.apply)
      case "NullPrimitive" => Right(NullPrimitive())
      case "Column" =>
        for {
          col <- c.get[Name]("column")
          table <- c.get[Option[Name]]("table")
        } yield Column(col, table)
      case "Arithmetic" =>
        for {
          lhs <- c.get[Expression]("lhs")
          op <- c.get[Arithmetic.Op]("op")
          rhs <- c.get[Expression]("rhs")
        } yield Arithmetic(lhs, op, rhs)
      case "Comparison" =>
        for {
          lhs <- c.get[Expression]("lhs")
          op <- c.get[Comparison.Op]("op")
          rhs <- c.get[Expression]("rhs")
        } yield Comparison(lhs, op, rhs)
      case "Function" =>
        for {
          name <- c.get[Name]("name")
          params <- c.get[Option[Seq[Expression]]]("params")
          distinct <- c.get[scala.Boolean]("distinct")
        } yield Function(name, params, distinct)
      case "WindowFunction" =>
        for {
          func <- c.get[Function]("function")
          partitionBy <- c.get[Option[Seq[Expression]]]("partitionBy")
          orderBy <- c.get[Seq[OrderBy]]("orderBy")
        } yield WindowFunction(func, partitionBy, orderBy)
      case "JDBCVar" => Right(JDBCVar())
      case "Subquery" => c.get[SelectBody]("query").map(Subquery.apply)
      case "CaseWhenElse" =>
        for {
          target <- c.get[Option[Expression]]("target")
          cases <- c.get[Seq[CirceJson]]("cases").flatMap { jsons =>
            jsons.traverse { json =>
              for {
                when <- json.hcursor.get[Expression]("when")
                thenExpr <- json.hcursor.get[Expression]("then")
              } yield (when, thenExpr)
            }
          }
          otherwise <- c.get[Expression]("otherwise")
        } yield CaseWhenElse(target, cases, otherwise)
      case "IsNull" => c.get[Expression]("target").map(IsNull.apply)
      case "Not" => c.get[Expression]("target").map(Not.apply)
      case "Cast" =>
        for {
          expr <- c.get[Expression]("expression")
          typeName <- c.get[Name]("typeName")
        } yield Cast(expr, typeName)
      case "InExpression" =>
        for {
          expr <- c.get[Expression]("expression")
          source <- c.get[CirceJson]("source").flatMap { json =>
            json.hcursor.get[String]("type").flatMap {
              case "Expressions" => json.hcursor.get[Seq[Expression]]("values").map(Left.apply)
              case "SelectBody" => json.hcursor.get[SelectBody]("query").map(Right.apply)
              case other => Left(DecodingFailure(s"Unknown InExpression source type: $other", json.hcursor.history))
            }
          }
        } yield InExpression(expr, source)
      case other => Left(DecodingFailure(s"Unknown Expression type: $other", c.history))
    }
  }

  // SelectTarget codecs (using circe-generic-extras)
  implicit val selectTargetEncoder: Encoder[SelectTarget] = deriveConfiguredEncoder[SelectTarget]
  implicit val selectTargetDecoder: Decoder[SelectTarget] = deriveConfiguredDecoder[SelectTarget]

  // OrderBy codec
  implicit val orderByEncoder: Encoder[OrderBy] = deriveEncoder[OrderBy]
  implicit val orderByDecoder: Decoder[OrderBy] = deriveDecoder[OrderBy]

  // FromElement codecs (using circe-generic-extras)
  implicit val fromElementEncoder: Encoder[FromElement] = deriveConfiguredEncoder[FromElement]
  implicit val fromElementDecoder: Decoder[FromElement] = deriveConfiguredDecoder[FromElement]

  // SelectBody codec (recursive)
  implicit val selectBodyEncoder: Encoder[SelectBody] = deriveEncoder[SelectBody]
  implicit val selectBodyDecoder: Decoder[SelectBody] = deriveDecoder[SelectBody]

  // ColumnAnnotation codecs (using circe-generic-extras)
  implicit val columnAnnotationEncoder: Encoder[ColumnAnnotation] = deriveConfiguredEncoder[ColumnAnnotation]
  implicit val columnAnnotationDecoder: Decoder[ColumnAnnotation] = deriveConfiguredDecoder[ColumnAnnotation]

  // PrimitiveValue codec (needed for ColumnDefinition)
  implicit val primitiveValueEncoder: Encoder[PrimitiveValue] = expressionEncoder.contramap(identity[Expression])
  implicit val primitiveValueDecoder: Decoder[PrimitiveValue] = expressionDecoder.emap {
    case pv: PrimitiveValue => Right(pv)
    case other => Left(s"Expected PrimitiveValue but got ${other.getClass.getSimpleName}")
  }

  // ColumnDefinition codec
  implicit val columnDefinitionEncoder: Encoder[ColumnDefinition] = deriveEncoder[ColumnDefinition]
  implicit val columnDefinitionDecoder: Decoder[ColumnDefinition] = deriveDecoder[ColumnDefinition]

  // TableAnnotation codecs (using circe-generic-extras)
  implicit val tableAnnotationEncoder: Encoder[TableAnnotation] = deriveConfiguredEncoder[TableAnnotation]
  implicit val tableAnnotationDecoder: Decoder[TableAnnotation] = deriveConfiguredDecoder[TableAnnotation]

  // InsertValues codecs (using circe-generic-extras)
  implicit val insertValuesEncoder: Encoder[InsertValues] = deriveConfiguredEncoder[InsertValues]
  implicit val insertValuesDecoder: Decoder[InsertValues] = deriveConfiguredDecoder[InsertValues]

  // WithClause codec
  implicit val withClauseEncoder: Encoder[WithClause] = deriveEncoder[WithClause]
  implicit val withClauseDecoder: Decoder[WithClause] = deriveDecoder[WithClause]

  // AlterViewAction codecs (using circe-generic-extras)
  implicit val alterViewActionEncoder: Encoder[AlterViewAction] = deriveConfiguredEncoder[AlterViewAction]
  implicit val alterViewActionDecoder: Decoder[AlterViewAction] = deriveConfiguredDecoder[AlterViewAction]

  // Statement codecs (base and common)
  implicit val statementEncoder: Encoder[Statement] = Encoder.instance {
    case EmptyStatement() => CirceJson.obj("type" -> CirceJson.fromString("EmptyStatement"))
    case u: Unparseable =>
      CirceJson.obj("type" -> CirceJson.fromString("Unparseable"), "content" -> CirceJson.fromString(u.content))
    case s: Select =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Select"),
        "body" -> s.body.asJson,
        "withClause" -> s.withClause.asJson
      )
    case u: Update =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Update"),
        "table" -> u.table.asJson,
        "set" -> u.set.map { case (n, e) => CirceJson.obj("name" -> n.asJson, "expression" -> e.asJson) }.asJson,
        "where" -> u.where.asJson
      )
    case d: Delete =>
      CirceJson.obj("type" -> CirceJson.fromString("Delete"), "table" -> d.table.asJson, "where" -> d.where.asJson)
    case i: Insert =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("Insert"),
        "table" -> i.table.asJson,
        "columns" -> i.columns.asJson,
        "values" -> i.values.asJson,
        "orReplace" -> CirceJson.fromBoolean(i.orReplace)
      )
    case ct: CreateTable =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("CreateTable"),
        "name" -> ct.name.asJson,
        "orReplace" -> CirceJson.fromBoolean(ct.orReplace),
        "columns" -> ct.columns.asJson,
        "annotations" -> ct.annotations.asJson
      )
    case cta: CreateTableAs =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("CreateTableAs"),
        "name" -> cta.name.asJson,
        "orReplace" -> CirceJson.fromBoolean(cta.orReplace),
        "query" -> cta.query.asJson
      )
    case cv: CreateView =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("CreateView"),
        "name" -> cv.name.asJson,
        "orReplace" -> CirceJson.fromBoolean(cv.orReplace),
        "query" -> cv.query.asJson,
        "materialized" -> CirceJson.fromBoolean(cv.materialized),
        "temporary" -> CirceJson.fromBoolean(cv.temporary)
      )
    case av: AlterView =>
      CirceJson.obj("type" -> CirceJson.fromString("AlterView"), "name" -> av.name.asJson, "action" -> av.action.asJson)
    case dt: DropTable =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("DropTable"),
        "name" -> dt.name.asJson,
        "ifExists" -> CirceJson.fromBoolean(dt.ifExists)
      )
    case dv: DropView =>
      CirceJson.obj(
        "type" -> CirceJson.fromString("DropView"),
        "name" -> dv.name.asJson,
        "ifExists" -> CirceJson.fromBoolean(dv.ifExists)
      )
    case e: Explain => CirceJson.obj("type" -> CirceJson.fromString("Explain"), "query" -> e.query.asJson)
    // Oracle statements
    case os: OracleStatement => oracleStatementEncoder(os)
  }

  implicit val statementDecoder: Decoder[Statement] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "EmptyStatement" => Right(EmptyStatement())
      case "Unparseable" => c.get[String]("content").map(Unparseable.apply)
      case "Select" =>
        for {
          body <- c.get[SelectBody]("body")
          withClause <- c.get[Seq[WithClause]]("withClause")
        } yield Select(body, withClause)
      case "Update" =>
        for {
          table <- c.get[Name]("table")
          set <- c.get[Seq[CirceJson]]("set").flatMap { jsons =>
            jsons.traverse { json =>
              for {
                name <- json.hcursor.get[Name]("name")
                expr <- json.hcursor.get[Expression]("expression")
              } yield (name, expr)
            }
          }
          where <- c.get[Option[Expression]]("where")
        } yield Update(table, set, where)
      case "Delete" =>
        for {
          table <- c.get[Name]("table")
          where <- c.get[Option[Expression]]("where")
        } yield Delete(table, where)
      case "Insert" =>
        for {
          table <- c.get[Name]("table")
          columns <- c.get[Option[Seq[Name]]]("columns")
          values <- c.get[InsertValues]("values")
          orReplace <- c.get[Boolean]("orReplace")
        } yield Insert(table, columns, values, orReplace)
      case "CreateTable" =>
        for {
          name <- c.get[Name]("name")
          orReplace <- c.get[Boolean]("orReplace")
          columns <- c.get[Seq[ColumnDefinition]]("columns")
          annotations <- c.get[Seq[TableAnnotation]]("annotations")
        } yield CreateTable(name, orReplace, columns, annotations)
      case "CreateTableAs" =>
        for {
          name <- c.get[Name]("name")
          orReplace <- c.get[Boolean]("orReplace")
          query <- c.get[SelectBody]("query")
        } yield CreateTableAs(name, orReplace, query)
      case "CreateView" =>
        for {
          name <- c.get[Name]("name")
          orReplace <- c.get[Boolean]("orReplace")
          query <- c.get[SelectBody]("query")
          materialized <- c.get[Boolean]("materialized")
          temporary <- c.get[Boolean]("temporary")
        } yield CreateView(name, orReplace, query, materialized, temporary)
      case "AlterView" =>
        for {
          name <- c.get[Name]("name")
          action <- c.get[AlterViewAction]("action")
        } yield AlterView(name, action)
      case "DropTable" =>
        for {
          name <- c.get[Name]("name")
          ifExists <- c.get[Boolean]("ifExists")
        } yield DropTable(name, ifExists)
      case "DropView" =>
        for {
          name <- c.get[Name]("name")
          ifExists <- c.get[Boolean]("ifExists")
        } yield DropView(name, ifExists)
      case "Explain" => c.get[SelectBody]("query").map(Explain.apply)
      case other => oracleStatementDecoderHelper(c, other)
    }
  }

  // Oracle-specific codecs
  // ReferentialAction codecs
  implicit val referentialActionEncoder: Encoder[ReferentialAction] = Encoder.encodeString.contramap(_.sql)
  implicit val referentialActionDecoder: Decoder[ReferentialAction] = Decoder.decodeString.emap {
    case "NO ACTION" => Right(ReferentialAction.NoAction)
    case "RESTRICT" => Right(ReferentialAction.Restrict)
    case "CASCADE" => Right(ReferentialAction.Cascade)
    case "SET NULL" => Right(ReferentialAction.SetNull)
    case "SET DEFAULT" => Right(ReferentialAction.SetDefault)
    case other => Left(s"Unknown ReferentialAction: $other")
  }

  // TableConstraint codecs
  implicit val tableConstraintEncoder: Encoder[TableConstraint] = deriveConfiguredEncoder[TableConstraint]
  implicit val tableConstraintDecoder: Decoder[TableConstraint] = deriveConfiguredDecoder[TableConstraint]

  // AlterTableAction codecs (using circe-generic-extras)
  implicit val alterTableActionEncoder: Encoder[AlterTableAction] = deriveConfiguredEncoder[AlterTableAction]
  implicit val alterTableActionDecoder: Decoder[AlterTableAction] = deriveConfiguredDecoder[AlterTableAction]

  implicit val columnModificationEncoder: Encoder[ColumnModification] = deriveEncoder[ColumnModification]
  implicit val columnModificationDecoder: Decoder[ColumnModification] = deriveDecoder[ColumnModification]

  implicit val storageClauseEncoder: Encoder[StorageClause] = deriveEncoder[StorageClause]
  implicit val storageClauseDecoder: Decoder[StorageClause] = deriveDecoder[StorageClause]

  // Custom codec for (Name, Expression) tuples to encode as objects with "name" and "expression" fields
  implicit val nameExpressionTupleEncoder: Encoder[(Name, Expression)] = Encoder.instance { case (name, expr) =>
    CirceJson.obj("name" -> name.asJson, "expression" -> expr.asJson)
  }
  implicit val nameExpressionTupleDecoder: Decoder[(Name, Expression)] = Decoder.instance { c =>
    for {
      name <- c.get[Name]("name")
      expr <- c.get[Expression]("expression")
    } yield (name, expr)
  }

  // Custom codec for Either[SelectBody, Seq[ColumnDefinition]] used in OracleCreateTable.schema
  // Encodes as an object with "type" field ("SelectBody" or "Columns") and corresponding data
  implicit val oracleCreateTableSchemaEncoder: Encoder[Either[SelectBody, Seq[ColumnDefinition]]] =
    Encoder.instance {
      case Left(query) =>
        CirceJson.obj("type" -> CirceJson.fromString("SelectBody"), "query" -> query.asJson)
      case Right(columns) =>
        CirceJson.obj("type" -> CirceJson.fromString("Columns"), "columns" -> columns.asJson)
    }
  implicit val oracleCreateTableSchemaDecoder: Decoder[Either[SelectBody, Seq[ColumnDefinition]]] =
    Decoder.instance { c =>
      c.get[String]("type").flatMap {
        case "SelectBody" => c.get[SelectBody]("query").map(Left.apply)
        case "Columns" => c.get[Seq[ColumnDefinition]]("columns").map(Right.apply)
        case other =>
          Left(DecodingFailure(s"Unknown OracleCreateTable schema type: $other", c.history))
      }
    }

  // OracleStatement codecs (using circe-generic-extras)
  // Note: The tuple codec above handles Update.set and OracleUpdate.set fields
  implicit val oracleStatementEncoder: Encoder[OracleStatement] = deriveConfiguredEncoder[OracleStatement]
  implicit val oracleStatementDecoder: Decoder[OracleStatement] = deriveConfiguredDecoder[OracleStatement]

  // Helper function to decode OracleStatement from Statement decoder
  def oracleStatementDecoderHelper(c: HCursor, typeName: String): Decoder.Result[Statement] = {
    // Use the automatic decoder for OracleStatement
    oracleStatementDecoder(c).left.map { failure =>
      DecodingFailure(s"Unknown Statement type: $typeName - ${failure.message}", c.history)
    }
  }
}
