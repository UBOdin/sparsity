package sparsity

import sparsity.oracle.OracleSQL
import sparsity.ansi.ANSISQL
import sparsity.postgres.PostgresSQL
import sparsity.common.parser.ErrorMessage
import sparsity.common.codec.CirceCodecs._
import fastparse._
import scala.io.Source
import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{FileSystems, Files, Paths}
import java.util.logging.{Level => JLevel, LogManager, Logger => JLogger}
import scribe.Logger
import scribe.Level
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.Printer
import io.circe.Encoder
import sparsity.common.statement.Statement
import com.arakelian.jq.{ImmutableJqLibrary, ImmutableJqRequest}
import mainargs.{arg, main, Leftover, Parser, TokensReader}
import sparsity.common.parser.SQLBaseObject

case class Annotation(file: String, line: Int, title: String, message: String, annotation_level: String)

object Annotation {
  implicit val encoder: Encoder[Annotation] = deriveEncoder[Annotation]
}

// Maps for dialect lookup - built dynamically using the name method
object SQLDialectMaps {
  val dialects: Seq[SQLBaseObject] = Seq(OracleSQL, ANSISQL, PostgresSQL)
  val dialectNames: Seq[String] = dialects.map(_.name)
  val nameToDialect: Map[String, SQLBaseObject] = dialects.map(d => d.name -> d).toMap
  val validDialects: String = dialects.map(_.name).mkString(", ")
}

// Custom parser for SQLBaseObject
object SQLBaseObjectReader extends TokensReader.Simple[SQLBaseObject] {
  def shortName = "dialect"
  def read(strs: Seq[String]): Either[String, SQLBaseObject] = {
    if (strs.isEmpty) {
      Left("Missing dialect name")
    } else {
      SQLDialectMaps.nameToDialect.get(strs.head.toLowerCase) match {
        case Some(dialect) => Right(dialect)
        case None => Left(s"Unknown dialect: ${strs.head}. Valid dialects are: ${SQLDialectMaps.validDialects}")
      }
    }
  }
}

// Custom parser for OutputFormat
object OutputFormatReader extends TokensReader.Simple[OutputFormat] {
  def shortName = "format"
  def read(strs: Seq[String]): Either[String, OutputFormat] = {
    if (strs.isEmpty) {
      Left("Missing output format")
    } else {
      val s = strs.head
      s match {
        case "text" => Right(TextFormat)
        case "json" => Right(StatementsJsonFormat)
        case s"jq($query)" => Right(JqFormat(query))
        case s"jq-file:$filePath" => Right(JqFileFormat(filePath))
        case _ => Left(s"Unknown output format: $s. Valid formats: text, json, jq(query), jq-file:path")
      }
    }
  }
}

// Output format types
sealed trait OutputFormat {
  def name: String
  def format(result: ProcessingResult): String
}

case object TextFormat extends OutputFormat {
  def name: String = "text"
  def format(result: ProcessingResult): String = {
    val textOutputLines = result.fileResults.flatMap { fileResult =>
      (s"\n=== Processing: ${fileResult.fileName}" :: fileResult.statementResults.flatMap { stmtResult =>
        val idx = stmtResult.index
        stmtResult.parseResult match {
          case Parsed.Success(_, _) =>
            List(s"✓ Statement ${idx + 1}: OK")
          case Parsed.Failure(label, index, _) =>
            val stmtIndex = index.toInt
            val absoluteLineNum = stmtResult.absoluteLineNumber.getOrElse(0)
            // For unparseable statements, show the full statement text instead of a substring
            val context = if (label == "unparseable") {
              // Show full statement text (up to 200 chars for readability)
              val fullText = stmtResult.context.statement
              if (fullText.length <= 200) fullText else fullText.take(200) + "..."
            } else {
              stmtResult.context.statement
                .substring(Math.max(0, stmtIndex - 50), Math.min(stmtResult.context.statement.length, stmtIndex + 50))
            }
            List(
              s"❌ ${stmtResult.context.relativePath}:${absoluteLineNum}: Syntax ERROR",
              s"Statement ${idx + 1} failed: $label",
              s"Context: $context"
            )
        }
      }) :+ fileResult.fileSummary
    }

    val overallSummary = s"\n=== Overall Summary ==="
    val summary2 = s"Files processed: ${result.fileResults.length}"
    val summary3 =
      s"Total: ${result.totalSuccessCount} successful, ${result.totalErrorCount} failed out of ${result.totalStatements} statements"

    (textOutputLines :+ overallSummary :+ summary2 :+ summary3).mkString("\n")
  }
}
case object StatementsJsonFormat extends OutputFormat {
  import ParsedEncoder._
  def name: String = "json"
  def format(result: ProcessingResult): String = {
    val statementsJson = result.asJson
    Printer.spaces2.copy(dropNullValues = true).print(statementsJson)
  }
}
case class JqFormat(query: String) extends OutputFormat {
  def name: String = "jq"
  def format(result: ProcessingResult): String = {
    // Generate statements JSON for JQ query
    val statementsJsonString = StatementsJsonFormat.format(result)
    try {
      // Suppress Java JQ library logging before initializing the library
      // This must be done before ImmutableJqLibrary.of() is called
      // Suppress all JQ-related loggers and their handlers
      val loggersToSuppress = Seq("com.arakelian.jq", "com.arakelian.jq.NativeLib", "com.arakelian.jq.JqLibrary")

      loggersToSuppress.foreach { loggerName =>
        val logger = JLogger.getLogger(loggerName)
        logger.setLevel(JLevel.OFF) // Completely disable logging
        logger.setUseParentHandlers(false)
        // Remove all handlers
        val handlers = logger.getHandlers
        handlers.foreach(logger.removeHandler)
      }

      val library = ImmutableJqLibrary.of()
      val request = ImmutableJqRequest
        .builder()
        .lib(library)
        .input(statementsJsonString)
        .filter(query)
        .build()
      request.execute().getOutput
    } catch {
      case e: Exception =>
        throw new RuntimeException(s"Error executing JQ query: ${e.getMessage}", e)
    }
  }
}
case class JqFileFormat(filePath: String) extends OutputFormat {
  def name: String = "jq-file"
  def format(result: ProcessingResult): String = {
    // Read the JQ query from the file
    val query =
      try {
        val currentDir = Paths.get(".").toAbsolutePath.normalize
        val queryFile = if (Paths.get(filePath).isAbsolute) {
          Paths.get(filePath).toFile
        } else {
          currentDir.resolve(filePath).toFile
        }

        if (!queryFile.exists() || !queryFile.isFile) {
          throw new RuntimeException(s"JQ query file not found: $filePath")
        }

        Source.fromFile(queryFile).mkString.trim
      } catch {
        case e: RuntimeException => throw e
        case e: Exception =>
          throw new RuntimeException(s"Error reading JQ query file '$filePath': ${e.getMessage}", e)
      }

    // Generate statements JSON for JQ query
    val statementsJsonString = StatementsJsonFormat.format(result)
    try {
      // Suppress Java JQ library logging before initializing the library
      // This must be done before ImmutableJqLibrary.of() is called
      // Suppress all JQ-related loggers and their handlers
      val loggersToSuppress = Seq("com.arakelian.jq", "com.arakelian.jq.NativeLib", "com.arakelian.jq.JqLibrary")

      loggersToSuppress.foreach { loggerName =>
        val logger = JLogger.getLogger(loggerName)
        logger.setLevel(JLevel.OFF) // Completely disable logging
        logger.setUseParentHandlers(false)
        // Remove all handlers
        val handlers = logger.getHandlers
        handlers.foreach(logger.removeHandler)
      }

      val library = ImmutableJqLibrary.of()
      val request = ImmutableJqRequest
        .builder()
        .lib(library)
        .input(statementsJsonString)
        .filter(query)
        .build()
      request.execute().getOutput
    } catch {
      case e: Exception =>
        throw new RuntimeException(s"Error executing JQ query from file '$filePath': ${e.getMessage}", e)
    }
  }
}

// Output destination types
sealed trait OutputDestination {
  def write(content: String): Unit
}
case object ConsoleDestination extends OutputDestination {
  def write(content: String): Unit = println(content)
}
case class FileDestination(path: String) extends OutputDestination {
  def write(content: String): Unit = {
    val writer = new PrintWriter(new File(path))
    try
      writer.println(content)
    finally
      writer.close()
  }
}

object OutputDestination {
  implicit val tokensReader: TokensReader.Simple[OutputDestination] = new TokensReader.Simple[OutputDestination] {
    def shortName = "destination"
    def read(strs: Seq[String]): Either[String, OutputDestination] = {
      if (strs.isEmpty) {
        Left("Missing output destination")
      } else {
        strs.head match {
          case "console" => Right(ConsoleDestination)
          case path: String => Right(FileDestination(path))
          case _ => Left(s"Unknown output destination: ${strs.head}. Valid destinations: console, file path")
        }
      }
    }
  }
}

@main(doc = "Parse SQL files using the specified dialect")
case class Config(
  @arg(positional = true, doc = "SQL dialect to use")
  dialect: SQLBaseObject,
  @arg(doc =
    "Output format. Format: text|json|jq(query)|jq-file:path. Example: --output jq(.files[].statements[]) or --output jq-file:pr-checker.jq"
  )
  output: OutputFormat = TextFormat,
  @arg(doc = "Output destination. Example: --write-to console or --write-to output.json")
  writeTo: OutputDestination = ConsoleDestination,
  @arg(doc = "SQL file(s) or glob pattern(s) to parse (e.g., *.sql)")
  filePatterns: Leftover[String]
)

case class StatementContext(
  statement: String,
  rawStatement: String,
  relativePath: String,
  dialectName: String,
  statementStartInContent: Int
)

object StatementContext {
  implicit val encoder: Encoder[StatementContext] = deriveEncoder[StatementContext]
}

case class StatementResult(
  index: Int,
  parseResult: Parsed[Statement],
  context: StatementContext,
  absoluteLineNumber: Option[Int] = None
) {
  def success: Boolean = parseResult match {
    case Parsed.Success(_, _) => true
    case _: Parsed.Failure => false
  }
}

// Custom encoder for Parsed[Statement] to make it serializable
object ParsedEncoder {
  implicit def parsedEncoder[T : Encoder]: Encoder[Parsed[T]] = Encoder.instance {
    case Parsed.Success(value, _) =>
      Json.obj("success" -> Json.fromBoolean(true), "value" -> value.asJson)
    case Parsed.Failure(label, index, extra) =>
      Json.obj("success" -> Json.fromBoolean(false), "label" -> Json.fromString(label), "index" -> Json.fromLong(index))
  }
}

object StatementResult {
  import ParsedEncoder._
  implicit val encoder: Encoder[StatementResult] = deriveEncoder[StatementResult]
}

case class FileResult(
  fileName: String,
  relativePath: String,
  statementResults: List[StatementResult],
  fileSummary: String
)

object FileResult {
  implicit val encoder: Encoder[FileResult] = deriveEncoder[FileResult]
}

case class ProcessingResult(
  fileResults: List[FileResult],
  totalSuccessCount: Int,
  totalErrorCount: Int,
  totalStatements: Int
)

object ProcessingResult {
  implicit val encoder: Encoder[ProcessingResult] = deriveEncoder[ProcessingResult]
}

object Main {
  // Configure scribe logger
  private val logger = Logger("sparsity.Main")

  // Make the custom parsers available implicitly
  implicit val sqlBaseObjectReader: TokensReader.Simple[SQLBaseObject] = SQLBaseObjectReader
  implicit val outputFormatReader: TokensReader.Simple[OutputFormat] = OutputFormatReader

  def main(args: scala.Array[String]): Unit = {
    // Parse command-line arguments using mainargs first to check output destination
    val config = Parser[Config].constructOrExit(args)

    // Configure logging - suppress logs when writing to console
    val shouldSuppressLogs = config.writeTo == ConsoleDestination
    val logLevel = if (shouldSuppressLogs) {
      // When writing to console, suppress INFO/DEBUG logs
      Option(System.getProperty("log.level"))
        .orElse(Option(System.getenv("LOG_LEVEL")))
        .getOrElse("WARN")
    } else {
      // When writing to file, respect user's log level preference
      Option(System.getProperty("log.level"))
        .orElse(Option(System.getenv("LOG_LEVEL")))
        .getOrElse("DEBUG")
    }

    val level = logLevel.toUpperCase match {
      case "TRACE" => Level.Trace
      case "DEBUG" => Level.Debug
      case "INFO" => Level.Info
      case "WARN" => Level.Warn
      case "ERROR" => Level.Error
      case _ => Level.Debug
    }

    // Configure root logger with console output and minimum level
    Logger.root
      .clearHandlers()
      .withHandler(minimumLevel = Some(level))
      .replace()

    // Suppress Java JQ library logging when writing to console
    if (shouldSuppressLogs) {
      // Suppress all JQ-related loggers early
      val loggersToSuppress = Seq("com.arakelian.jq", "com.arakelian.jq.NativeLib", "com.arakelian.jq.JqLibrary")

      loggersToSuppress.foreach { loggerName =>
        val logger = JLogger.getLogger(loggerName)
        logger.setLevel(JLevel.OFF)
        logger.setUseParentHandlers(false)
      }
    }

    // Expand glob patterns and collect all files
    val files = config.filePatterns.value.flatMap { pattern =>
      val currentDir = Paths.get(".").toAbsolutePath.normalize

      // If it's a direct file path and exists, return it
      val directFile = if (Paths.get(pattern).isAbsolute) {
        Paths.get(pattern).toFile
      } else {
        currentDir.resolve(pattern).toFile
      }

      if (directFile.exists() && directFile.isFile && !pattern.contains('*') && !pattern.contains('?')) {
        Seq(directFile)
      } else if (pattern.contains('*') || pattern.contains('?')) {
        // Normalize path separators to forward slashes for glob matching
        val normalizedPattern = pattern.replace('\\', '/')

        // Determine the base directory to start searching from
        // For patterns like "../**/*oracle*/**/*.sql", find the deepest non-glob directory
        val parts = normalizedPattern.split("/").filter(_.nonEmpty)
        def findBaseRec(path: java.nio.file.Path, index: Int): (java.nio.file.Path, Int) =
          if (index >= parts.length) {
            (path, index)
          } else {
            val part = parts(index)
            if (part == "..") {
              findBaseRec(path.getParent, index + 1)
            } else if (part == ".") {
              findBaseRec(path, index + 1)
            } else if (part.contains('*') || part.contains('?')) {
              (path, index) // Stop here, found glob
            } else {
              val nextPath = path.resolve(part)
              if (nextPath.toFile.exists() && nextPath.toFile.isDirectory) {
                findBaseRec(nextPath, index + 1)
              } else {
                (path, index) // Stop here, directory doesn't exist
              }
            }
          }

        val (baseDir, baseIndex) = findBaseRec(currentDir, 0)

        // Build the relative pattern from the base directory
        val relativePattern = if (baseIndex < parts.length) {
          parts.slice(baseIndex, parts.length).mkString("/")
        } else {
          "**" // If we consumed everything, search recursively
        }

        val basePath = baseDir.normalize

        if (basePath.toFile.exists() && basePath.toFile.isDirectory) {
          val fs = FileSystems.getDefault
          val matcher = fs.getPathMatcher(s"glob:$relativePattern")

          // Walk the directory tree recursively using Stream
          try {
            import scala.jdk.CollectionConverters._
            Files
              .walk(basePath)
              .iterator()
              .asScala
              .filter(_.toFile.isFile)
              .filter { path =>
                val relativePath = basePath.relativize(path).toString.replace('\\', '/')
                matcher.matches(Paths.get(relativePath))
              }
              .map(_.toFile)
              .toSeq
          } catch {
            case _: Exception =>
              Seq.empty
          }
        } else {
          Seq.empty
        }
      } else {
        // Not a glob pattern, but file doesn't exist
        Seq.empty
      }
    }.distinct

    if (files.isEmpty) {
      logger.warn(s"No files found matching: ${config.filePatterns.value.mkString(", ")}")
      System.exit(1)
    }

    // Step 1: Run the parser
    val result = parseFiles(files, config.dialect)

    // Step 2: Convert results into outputs
    generateOutputs(result, config)

    // Exit with error code if there were errors
    if (result.totalErrorCount > 0) {
      System.exit(1)
    }
  }

  /** Parses a list of SQL files using the specified dialect and returns processing results. This method is public to
    * enable testing.
    *
    * @param files
    *   The list of files to parse
    * @param dialect
    *   The SQL dialect object to use
    * @return
    *   ProcessingResult containing file results and summary statistics
    */
  def parseFiles(files: Seq[File], dialect: SQLBaseObject): ProcessingResult = {
    val fileResults = files.map { file =>
      val relativePath = {
        val currentDir = Paths.get(".").toAbsolutePath.normalize
        val filePathObj = file.toPath.toAbsolutePath.normalize
        try
          currentDir.relativize(filePathObj).toString.replace('\\', '/')
        catch {
          case _: Exception => file.getPath
        }
      }

      val content = Source.fromFile(file).mkString

      val dialectName = dialect.name

      // Use parseAll to parse all statements with error recovery (now returns positions)
      val parseResults: Seq[sparsity.common.statement.StatementParseResult] = dialect.parseAll(content)

      // Convert StatementParseResult to StatementResult
      val statementResults = parseResults.zipWithIndex.map { case (parseResult, idx) =>
        val (statementText, isUnparseable) = parseResult.result match {
          case Right(stmt) => (stmt.toString, false)
          case Left(unparseable) => (unparseable.content, true)
        }

        // Calculate absolute line number from start position
        val absoluteLineNum = if (parseResult.startPos >= 0 && parseResult.startPos < content.length) {
          val contentBeforeStmt = content.substring(0, parseResult.startPos)
          val linesBeforeStmt = contentBeforeStmt.count(_ == '\n')
          Some(linesBeforeStmt + 1) // +1 because line numbers are 1-based
        } else {
          None
        }

        // Create parse result object
        val finalParseResult = parseResult.result match {
          case Right(stmt) => Parsed.Success(stmt, 0)
          case Left(_) => Parsed.Failure("unparseable", 0, null)
        }

        val stmtContext = StatementContext(
          statement = statementText,
          rawStatement = statementText,
          relativePath = relativePath,
          dialectName = dialectName,
          statementStartInContent = parseResult.startPos
        )

        val stmtResult = StatementResult(
          index = idx,
          parseResult = finalParseResult,
          context = stmtContext,
          absoluteLineNumber = absoluteLineNum
        )

        // Log parsing details for failures
        parseResult.result match {
          case Left(unparseable) =>
            val stmtLines = statementText.split("\n")
            logger.info(s"  Label: unparseable")
            logger.info(s"  Context: ${statementText.take(100)}")
            logger.debug(s"  Full statement:")
            logger.debug(s"  ${stmtLines.mkString("\n  ")}")
          case Right(_) =>
        }

        stmtResult
      }.toList

      val successCount = statementResults.count(_.success)
      val errorCount = statementResults.count(!_.success)
      val summary =
        s"File summary: $successCount successful, $errorCount failed out of ${statementResults.length} statements"

      FileResult(
        fileName = file.getName,
        relativePath = relativePath,
        statementResults = statementResults,
        fileSummary = summary
      )
    }.toList

    val totals = fileResults.foldLeft((0, 0, 0)) { case ((success, error, total), fileResult) =>
      val stmtCounts = fileResult.statementResults.foldLeft((0, 0)) { case ((s, e), stmt) =>
        if (stmt.success) (s + 1, e) else (s, e + 1)
      }
      (success + stmtCounts._1, error + stmtCounts._2, total + fileResult.statementResults.length)
    }

    ProcessingResult(
      fileResults = fileResults,
      totalSuccessCount = totals._1,
      totalErrorCount = totals._2,
      totalStatements = totals._3
    )
  }

  /** Generates all outputs from parsing results. This method is public to enable testing.
    *
    * @param result
    *   The processing result from parsing files
    * @param config
    *   The configuration containing output options
    */
  def generateOutputs(result: ProcessingResult, config: Config): Unit = {
    // Process each output format (all outputs go to console)
    val formattedOutput = config.output.format(result)
    config.writeTo.write(formattedOutput)
  }
}
