package sparsity

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.core.Fragments
import sparsity.statement._
import sparsity._
import fastparse.Parsed

import scala.io._
import java.io._

class SQLParserSpec extends Specification 
{

  def testSelect[R](s: String)(body: Select => R) = {
    SQLParser(s) match {
      case Parsed.Success(result, index) => 
        body(result.asInstanceOf[SelectStatement].body)
      case Parsed.Failure(error, index, extra) => 
        throw new RuntimeException(error)
    }
  }
  def streamSelect[R](input: Reader):Iterator[Select] = {
    SQLParser(input).map { 
      case Parsed.Success(result, index) => 
        result.asInstanceOf[SelectStatement].body
      case f@Parsed.Failure(error, index, extra) =>
        throw new RuntimeException(f.msg)
    }
  }

  def e = ExpressionParser(_)
  def et(s:String):SelectTarget = SelectExpression(e(s))
  def f(t:Name):FromElement                 = FromTable(None, t, None)
  def f(t:Name, a:Name):FromElement         = FromTable(None, t, Some(a))
  def f(t:Name, a:Name, s:Name):FromElement = FromTable(Some(s), t, Some(a))
  def asc(s:String) = OrderBy(e(s), true)
  def desc(s:String) = OrderBy(e(s), false)

  implicit def StringToName(s:String) = Name(s)

  "The SELECT Parser" should {

    "Parse basic SELECT queries" >> {
      testSelect("SELECT 1;") { q => 
        q.target should contain(exactly(
          et("1")
        ))
      }

      testSelect("SELECT A FROM R;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
      }

      testSelect("SELECT A, 1 FROM R AS Foo, S.Q AS Bar;") { q => 
        q.target should contain(exactly(
          et("A"),
          et("1")
        ))
        q.from should contain(exactly(
          f("R", "Foo"),
          f("Q", "Bar", "S")
        ))
      }

      testSelect("SELECT A FROM R WHERE B = 1;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.where should beEqualTo( Some(e("B=1")) )
      }

      testSelect("SELECT A FROM R ORDER BY A, B DESC;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.orderBy should beEqualTo( Seq(asc("A"), desc("B")) ) 
      }

      testSelect("SELECT A FROM R ORDER BY A, B LIMIT 5;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.orderBy should beEqualTo( Seq(asc("A"), asc("B")) ) 
        q.limit should beEqualTo( Some(5) )
      }

      testSelect("SELECT A FROM R ORDER BY A, B OFFSET 5;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.orderBy should beEqualTo( Seq(asc("A"), asc("B")) ) 
        q.offset should beEqualTo( Some(5) )
      }

      testSelect("SELECT A FROM R ORDER BY A, B LIMIT 5 OFFSET 5;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.orderBy should beEqualTo( Seq(asc("A"), asc("B")) ) 
        q.offset should beEqualTo( Some(5) )
        q.limit should beEqualTo( Some(5) )
      }
    }

    "Parse aggregate SELECT queries" >> {
      testSelect("SELECT A FROM R GROUP BY A;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.groupBy should beEqualTo(Some(Seq(e("A"))))
      }

      testSelect("SELECT A FROM R GROUP BY A HAVING COUNT(*) > 10;") { q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.groupBy should beEqualTo(Some(Seq(e("A"))))
        q.having should beEqualTo(Some(e("COUNT(*) > 10")))
      }
    }

    "Parse Union queries" >> {
      testSelect("SELECT A FROM R UNION ALL SELECT A FROM S;"){ q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(f("R")))
        q.union must not beNone
        val (unionType, q2) = q.union.get
        unionType should be equalTo(Union.All)
        q2.target should contain(exactly(et("A")))
        q2.from should contain(exactly(f("S")))
      }
    }

    "Parse Nested queries" >> {
      testSelect("SELECT A FROM (SELECT A FROM R) Q;"){ q => 
        q.target should contain(exactly(et("A")))
        q.from should contain(exactly(
          FromSelect(
            Select(
              target = Seq(et("A")), 
              from = Seq(f("R"))
            ),
            "Q"
          ):FromElement
        ))
      }
    }

    "Parse a sequence of queries" >> {
      val queryStream = Source.fromFile("src/test/assets/queries.sql").bufferedReader
      val selects = streamSelect(queryStream)

      selects.next.target should contain(exactly(et("SUM(A)")))
      selects.next.target should contain(exactly(et("AVG(A)")))
      selects.next.target should contain(exactly(et("MIN(A)")))
      
    }

  }
}
