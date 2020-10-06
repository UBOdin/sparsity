package sparsity

import org.specs2.mutable.Specification
import fastparse.parse
import sparsity.parser.Json
import fastparse.Parsed
import org.specs2.specification.core._

class JsonSpec extends Specification
{

  "parse paths" >> {
    Fragment.foreach(Seq(
      "$",
      "$.foo",
      "$.`foo`",
      "$.*",
      "$[1]",
      "$[1 TO 23]",
      "$[*]",
      "$[1,2]",
      "$[1,2 TO 9]",
      "$[1,2 TO 9,3]",
      "$.*.*.foo",
      "$[1].foo[*].bar",
    )) { path => 
      (path) >> { 
        parse(path, Json.path(_)) match {
          case err:Parsed.Failure => 
            ko(s"Path parse failure: ${err.extra.trace().longAggregateMsg}")
          case Parsed.Success(result, _) => 
            result.toString must beEqualTo(path)
        }
      }
    }
  }

  "parse expressions" >> {
    Fragment.foreach(Seq(
      """
        JSON_TABLE(foo, $.bar) COLUMNS(
          bla varchar PATH $.bla, 
          baz varchar EXISTS PATH $.baz,
          hesh varchar FORMAT JSON WITH UNCONDITIONAL ARRAY WRAPPER PATH $.hesh[*],
          NESTED PATH $.foozball COLUMNS(
            cookie varchar PATH $.cookie
          ),
          idx FOR ORDINALITY
        )
      """,
      "JSON_QUERY(foo, $.bar.* WITH UNCONDITIONAL ARRAY WRAPPER)",
      "JSON_VALUE(foo, $.bar.*)",
      "JSON_EXISTS(foo, $.bar.*)",
    )) { expression =>
      expression >> {
        parse(expression, Json.expression(_)) match {
          case err:Parsed.Failure => 
            ko(s"Path parse failure: ${err.extra.trace().longAggregateMsg}")
          case Parsed.Success(result, _) => 
            result.toString must beEqualTo(expression).ignoreSpace
        }
      }
    }
  }

}