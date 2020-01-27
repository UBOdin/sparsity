package sparsity

import org.specs2.mutable._
import org.specs2.specification._

import fastparse.{ parse, Parsed }

import sparsity.parser.{ SQL, ErrorMessage }

class ErrorMessageSpec extends Specification
{
  "The error message formatter" >> {

    "Format SELECT typos reasonably" >> {
        val query = "SEALECT foo \nFROM bar;"
        val result = parse(
                        query, 
                        SQL.select(_), 
                        verboseFailures = true
                      )

        result must beAnInstanceOf[Parsed.Failure]
        ErrorMessage.format(
          query, 
          result.asInstanceOf[Parsed.Failure]
        ) must beEqualTo(
            query.split("\n")(0) + "\n" +
            "^--- expected SELECT"
        )
    }

  }
}