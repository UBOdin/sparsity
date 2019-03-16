package sparsity

import org.specs2.mutable.Specification

class ExpressionParserSpec extends Specification 
{
  "The Expression Parser" should {

    "Parse Comparisons" >> {
      ExpressionParser("1 > 2") should be equalTo(
        Comparison(LongPrimitive(1), Comparison.Gt, LongPrimitive(2))
      )
    }

    "Parse Arithmetic" >> {
      ExpressionParser("1+2") should be equalTo(
        Arithmetic(LongPrimitive(1), Arithmetic.Add, LongPrimitive(2))
      )
      ExpressionParser("1 + 2") should be equalTo(
        Arithmetic(LongPrimitive(1), Arithmetic.Add, LongPrimitive(2))
      )
    }

    "Respect Order of Operations" >> {
      ExpressionParser("1+2*3") should be equalTo(
        Arithmetic(LongPrimitive(1), Arithmetic.Add, 
          Arithmetic(LongPrimitive(2), Arithmetic.Mult, LongPrimitive(3))
        )
      )
      ExpressionParser("1+2 > 3") should be equalTo(
        Comparison(
          Arithmetic(LongPrimitive(1), Arithmetic.Add, LongPrimitive(2)),
          Comparison.Gt, LongPrimitive(3)
        )
      )
      ExpressionParser("1+2 > 3 AND true") should be equalTo(
        Arithmetic(
          Comparison(
            Arithmetic(LongPrimitive(1), Arithmetic.Add, LongPrimitive(2)),
            Comparison.Gt, LongPrimitive(3)
          ),
          Arithmetic.And,
          BooleanPrimitive(true)
        )
      )
    }

    "Parse NULL tests" >> {
      ExpressionParser("1 IS NULL") should be equalTo(
        IsNull(LongPrimitive(1))
      )
      ExpressionParser("1 IS NOT NULL") should be equalTo(
        Not(IsNull(LongPrimitive(1)))
      )
    }

    "Parse Variables" >> {
      ExpressionParser("A") should be equalTo(
        Column(Name("A"))
      )
      ExpressionParser("R.A") should be equalTo(
        Column(Name("A"), Some(Name("R")))
      )
      ExpressionParser("`A`") should be equalTo(
        Column(Name("A", true))
      )
      ExpressionParser("`R`.A") should be equalTo(
        Column(Name("A"), Some(Name("R", true)))
      )
      ExpressionParser("Foo.`Bar`") should be equalTo(
        Column(Name("Bar", true), Some(Name("Foo")))
      )
      ExpressionParser("int") should be equalTo(
        Column(Name("int"))
      )
    }

    "Parse Strings" >> {
      ExpressionParser("'Foo'") should be equalTo(StringPrimitive("Foo"))
      ExpressionParser("'Foo''sball'") should be equalTo(StringPrimitive("Foo'sball"))
    }

    "Parse CASE Expressions" >> {
      ExpressionParser("CASE WHEN A > 1 THEN B ELSE C END") should be equalTo(
        CaseWhenElse(
          None,
          Seq( Comparison(
                  Column(Name("A")), 
                  Comparison.Gt, 
                  LongPrimitive(1)
                ) -> Column(Name("B"))
          ),
          Column(Name("C"))
        )
      )
      ExpressionParser("CASE A WHEN 1 THEN B WHEN 2 THEN C ELSE D END") should be equalTo(
        CaseWhenElse(
          Some(Column(Name("A"))),
          Seq( 
            LongPrimitive(1) -> Column(Name("B")),
            LongPrimitive(2) -> Column(Name("C"))
          ),
          Column(Name("D"))
        )
      )
    }

  }
}
