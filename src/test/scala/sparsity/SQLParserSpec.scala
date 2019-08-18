package sparsity

import org.specs2.matcher.MatchResult
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.core.Fragments
import sparsity.statement._
import sparsity.select._
import sparsity.parser.{SQL, Expression}
import fastparse.Parsed
import sparsity.expression.{LongPrimitive, StringPrimitive, Column}

import scala.io._
import java.io._

class SQLParserSpec extends Specification 
{

  def testSelect(s: String)(body: SelectBody => MatchResult[_]) = {
    SQL(s) match {
      case Parsed.Success(result, index) => 
        body(result.asInstanceOf[Select].body)
      case f@Parsed.Failure(error, index, extra) => 
        throw new RuntimeException(f.trace().longMsg)
    }
  }
  def statement[Q](s: String)(body: Q => MatchResult[_]) = 
  {
    SQL(s) match{
      case Parsed.Success(result, index) => 
        body(result.asInstanceOf[Q])
      case f@Parsed.Failure(error, index, extra) => 
        throw new RuntimeException(f.trace().longMsg)
    }
  }
  def streamSelect(input: Reader):Iterator[SelectBody] = {
    SQL(input).map { 
      case Parsed.Success(result, index) => 
        result.asInstanceOf[Select].body
      case f@Parsed.Failure(error, index, extra) =>
        throw new RuntimeException(f.longMsg)
    }
  }

  def e = Expression(_)
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

      testSelect("SELECT DOB FROM DetectSeriesTest2 WHERE Rank = 1 ORDER BY DOB;") { q =>
        q.orderBy should beEqualTo( Seq(asc("DOB")) )
      }
    }

    "Parse SELECT queries with partial keywords" >> 
    {
      testSelect("SELECT NOTE FROM R;") { q =>
        q.target should contain(exactly(SelectExpression(Column("NOTE")):SelectTarget))
      }
      testSelect("SELECT CASE_NUMBER FROM R;") { q =>
        q.target should contain(exactly(SelectExpression(Column("CASE_NUMBER")):SelectTarget))
      }
    }

    "Parse SELECT queries with quoted identifiers" >> 
    {
      testSelect("SELECT `RANGE` AS `RANGE` FROM R;") { q =>
        q.target should contain(SelectExpression(e("`RANGE`"), Some(Name("RANGE", true))))
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

      testSelect("SELECT A, SUM(B) FROM R GROUP BY A;") { q =>
        q.target should contain(exactly(et("A"), et("SUM(B)")))
        q.groupBy should beEqualTo(Some(Seq(e("A"))))
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
            SelectBody(
              target = Seq(et("A")), 
              from = Seq(f("R"))
            ),
            "Q"
          ):FromElement
        ))
      }
    }

    "Parse JOIN queries" >> {
      testSelect("SELECT R.A, S.C FROM R NATURAL JOIN S ON R.B = S.B;"){ q => 
        q.from(0) should beAnInstanceOf[FromJoin]
      }
      testSelect("SELECT R.A, S.C FROM R JOIN S ON R.B = S.B;"){ q => 
        q.from(0) should beAnInstanceOf[FromJoin]
      }
    }

    "Parse a sequence of queries" >> {
      val queryStream = Source.fromFile("src/test/resources/queries.sql").bufferedReader
      val selects = streamSelect(queryStream)

      selects.next.target should contain(exactly(et("SUM(A)")))
      selects.next.target should contain(exactly(et("AVG(A)")))
      selects.next.target should contain(exactly(et("MIN(A)")))
      selects.next.target should contain(exactly(et("MAX(A)")))

      while(selects.hasNext) { selects.next }
      ok
      
    }

    "Parse multiline queries" >> {
      testSelect("""SELECT A, B FROM R
  UNION
SELECT A, B FROM R
  UNION
SELECT A, B FROM R;""") { q => 
        q.union should not be equalTo(None)
        q.union.get._2.union should not be equalTo(None)
      }
    }

    "Parse queries with quoted strings" >> {
      testSelect("""SELECT '''foo''';""") { q =>
        q.target should contain(exactly(SelectExpression(StringPrimitive("'foo'")):SelectTarget))
      }

    }

    "Parse queries with schema-ed tables" >> {
      testSelect("""SELECT A FROM schema.R;""") { q => 
        q.from should contain(exactly(FromTable(Some(Name("schema")), Name("R"), None):FromElement))
      }

    }
  }

  "The Statement Parser" should {

    "parse UPDATE statements" >> {

      statement[Update]("UPDATE foo SET bar = foo.baz + 2 WHERE foo.zing < 2;") 
      { stmt => 
          stmt.table must beEqualTo("foo")
          stmt.set must contain(exactly((Name("bar"), e("foo.baz + 2"))))
          stmt.where must beEqualTo(Some(e("foo.zing < 2")))
      }
      statement[Update]("UPDATE foo SET bar = foo.baz + 2;")
      { stmt =>
          stmt.table must beEqualTo("foo")
          stmt.set must contain(exactly((Name("bar"), e("foo.baz + 2"))))
          stmt.where must beEqualTo(None)
      }
      statement[Update]("UPDATE foo SET bar = foo.baz + 2, zing = 29;")
      { stmt =>
          stmt.table must beEqualTo("foo")
          stmt.set must contain(exactly(
            (Name("bar"), e("foo.baz + 2")),
            (Name("zing"), e("29"))
          ))
      }

    }

    "parse DELETE statements" >> {

      statement[Delete]("DELETE FROM foo;"){ stmt =>
        stmt.table must beEqualTo("foo")
        stmt.where must beEqualTo(None)
      }
      statement[Delete]("DELETE FROM foo WHERE baz<2;"){ stmt =>
        stmt.table must beEqualTo("foo")
        stmt.where must beEqualTo(Some(e("baz < 2")))
      }

    }

    "parse INSERT statements" >> {

      statement[Insert]("INSERT INTO foo(bar, baz) VALUES (1, 2);"){ stmt =>
        stmt.table must beEqualTo("foo")
        stmt.columns.get must contain(exactly(
          Name("bar"), Name("baz")
        ))
        stmt.values must beEqualTo(
          ExplicitInsert(Seq(Seq(
            LongPrimitive(1), LongPrimitive(2)
          )))
        )
        stmt.orReplace must beEqualTo(false)
      }
      statement[Insert]("INSERT INTO foo(bar) VALUES (1), (2);"){ stmt =>
        stmt.table must beEqualTo("foo")
        stmt.columns.get must contain(exactly(
          Name("bar")
        ))
        stmt.values must beEqualTo(
          ExplicitInsert(Seq(Seq(
            LongPrimitive(1)
          ), Seq(
            LongPrimitive(2)
          )))
        )
        stmt.orReplace must beEqualTo(false)
      }
      statement[Insert]("INSERT OR REPLACE INTO foo SELECT 1 FROM foo;"){ stmt =>
        stmt.table must beEqualTo("foo")
        stmt.columns must beEqualTo(None)
        stmt.values must beEqualTo(
          SelectInsert(SelectBody(
            target = Seq(et("1")),
            from = Seq(f("foo"))
          ))
        )
        stmt.orReplace must beEqualTo(true)
      }

    }

    "parse CREATE TABLE statements" >> {

      statement[CreateTable]("""
        CREATE OR REPLACE TABLE foo(
          bar int, 
          baz string DEFAULT 'foo', 
          broz int NOT NULL PRIMARY KEY, 
          INDEX ON baz, 
          INDEX ON (foo, baz)
        );""") { stmt =>
        stmt.name must beEqualTo(Name("foo"))
        stmt.columns must contain(exactly(
          ColumnDefinition(Name("bar"), "int"),
          ColumnDefinition(Name("baz"), "string", annotations = Seq(
            ColumnDefaultValue(e("'foo'"))
          )),
          ColumnDefinition(Name("broz"), "int", annotations = Seq(
            ColumnIsNotNullable(),
            ColumnIsPrimaryKey()
          ))
        ))
        stmt.annotations must contain(exactly(
          TableIndexOn(Seq(Name("baz"))):TableAnnotation,
          TableIndexOn(Seq(Name("foo"), Name("baz")))
        ))
      }
    }

    "parse CREATE TABLE AS statements" >> {
      statement[CreateTableAs]("""
        CREATE TABLE foo AS SELECT * FROM bar;
      """) { stmt =>
        stmt.name must beEqualTo(Name("foo"))
        stmt.query.from must contain(exactly(f("bar")))
      }
    }

    "parse CREATE VIEW statements" >> {
      statement[CreateView](
        "CREATE VIEW foo AS SELECT * FROM bar;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
        stmt.query.from must contain(exactly(f("bar")))
        stmt.orReplace must beFalse
        stmt.temporary must beFalse
      }
      statement[CreateView](
        "CREATE OR REPLACE VIEW foo AS SELECT * FROM bar;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
        stmt.query.from must contain(exactly(f("bar")))
        stmt.orReplace must beTrue
        stmt.temporary must beFalse
      }
    }

    "parse CREATE TEMPORARY VIEW statements" >> {
      statement[CreateView](
        "CREATE TEMPORARY VIEW foo AS SELECT * FROM bar;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
        stmt.query.from must contain(exactly(f("bar")))
        stmt.orReplace must beFalse
        stmt.temporary must beTrue
      }
      statement[CreateView](
        "CREATE OR REPLACE VIEW foo AS SELECT * FROM bar;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
        stmt.query.from must contain(exactly(f("bar")))
        stmt.orReplace must beTrue
      }
    }

    "parse DROP TABLE statements" >> {
      statement[DropTable](
        "DROP TABLE foo;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
      }
      statement[DropTable](
        "DROP TABLE IF EXISTS foo;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
      }
    }

    "parse DROP VIEW statements" >> {
      statement[DropView](
        "DROP VIEW foo;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
      }
      statement[DropView](
        "DROP VIEW IF EXISTS foo;"
      ) { stmt => 
        stmt.name must beEqualTo(Name("foo"))
      }
    }

  }
}
