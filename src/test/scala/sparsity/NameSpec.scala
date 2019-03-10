package sparsity

import org.specs2.mutable.Specification

class NameSpec extends Specification 
{

  "Sparsity Names" should { 

    "Support Case-Insensitive Comparison" >> {

      Name("bob") should be equalTo(Name("BOB"))
      Name("bob") should be equalTo(Name("Bob"))
      Name("Bob") should be equalTo(Name("Bob"))

    }

    "Support Case-Sensitive Comparison" >> {
      Name("bob", true) should be equalTo(Name("bob"))
      Name("bob", true) should be equalTo(Name("bob", true))
      Name("bob") should be equalTo(Name("bob", true))

      Name("bob", true) should not be equalTo(Name("Bob"))
      Name("bob", true) should not be equalTo(Name("Bob", true))
      Name("bob") should not be equalTo(Name("Bob", true))
    }

    "Support Map Lookups" >> {
      val data = Map[Name,String](
        Name("bob") -> "SOMETHING",
        Name("alice", true) -> "SOMETHINGELSE"
      )

      data should haveKey(Name("bob"))
      data should haveKey(Name("BOB"))
      data should not haveKey(Name("BOB", true))

      data should haveKey(Name("alice"))
      data should not haveKey(Name("Alice"))

      data.get(Name("bob")) should be equalTo(Some("SOMETHING"))
      data.get(Name("Bob")) should be equalTo(Some("SOMETHING"))
      data.get(Name("bob", true)) should be equalTo(Some("SOMETHING"))
      data.get(Name("Bob", true)) should be equalTo(None)

      data.get(Name("alice")) should be equalTo(Some("SOMETHINGELSE"))
      data.get(Name("Alice")) should be equalTo(None)
    }

  }

}