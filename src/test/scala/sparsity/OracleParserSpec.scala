package sparsity

import org.specs2.mutable._
import sparsity.oracle.{OracleSQL, OracleStatement}
import sparsity.common.statement.{Statement, StatementParseResult, Unparseable}
import fastparse.Parsed
import scala.io.Source

class OracleParserSpec extends Specification {

  "The Oracle SQL Parser" should {
    "parse all statements from oracle.sql" in {
      val content = Source.fromFile("src/test/resources/oracle.sql").mkString

      val parseResult = OracleSQL.parseAll(content)
      val expectedFailures = "Some random text that should fail to parse.\n/"

      val failures = parseResult.zipWithIndex
        .collect {
          case (StatementParseResult(Left(Unparseable(content)), _, _), idx) if content != expectedFailures =>
            s"Statement ${idx + 1}: Failed to parse\n`${content}`"
        }
        .mkString("\n")
      failures must beEqualTo("")
    }
  }
}
