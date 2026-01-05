package sparsity

import org.specs2.mutable.Specification

object NameSpecUtils
{
  var testData = Map[Name,String](
        Name("bob") -> "SOMETHING",
        Name("alice", true) -> "SOMETHINGELSE"
      )
  var testMap = (name: Name) => testData.get(name)
}

class NameSpec extends Specification
{


  "Sparsity Names" should {

    "Support Case-Insensitive Comparison" >> {

      Name("bob") should beEqualTo(Name("BOB"))
      Name("bob") should beEqualTo(Name("Bob"))
      Name("Bob") should beEqualTo(Name("Bob"))

    }

    "Support Case-Sensitive Comparison" >> {
      Name("bob", true) should beEqualTo(Name("bob"))
      Name("bob", true) should beEqualTo(Name("bob", true))
      Name("bob") should beEqualTo(Name("bob", true))

      Name("bob", true) should not(beEqualTo(Name("Bob")))
      Name("bob", true) should not(beEqualTo(Name("Bob", true)))
      Name("bob") should not(beEqualTo(Name("Bob", true)))
    }

    "Support Map Lookups" >> {
      val data = Map[Name,String](
        Name("bob") -> "SOMETHING",
        Name("alice", true) -> "SOMETHINGELSE"
      )

      data should haveKey(Name("bob"))
      data should haveKey(Name("BOB"))
      data should not(haveKey(Name("BOB", true)))

      data should haveKey(Name("alice"))
      data should not(haveKey(Name("Alice")))

      data.get(Name("bob")) should beEqualTo(Some("SOMETHING"))
      data.get(Name("Bob")) should beEqualTo(Some("SOMETHING"))
      data.get(Name("bob", true)) should beEqualTo(Some("SOMETHING"))
      data.get(Name("Bob", true)) should beEqualTo(None)

      data.get(Name("alice")) should beEqualTo(Some("SOMETHINGELSE"))
      data.get(Name("Alice")) should beEqualTo(None)
    }

    "Support Map Lookups Even When Forced" >> {
      NameSpecUtils.testMap(Name("bob")) should beEqualTo(Some("SOMETHING"))
      NameSpecUtils.testMap(Name("Bob")) should beEqualTo(Some("SOMETHING"))
      NameSpecUtils.testMap(Name("Bob", true)) should beEqualTo(None)
      NameSpecUtils.testMap(Name("alice")) should beEqualTo(Some("SOMETHINGELSE"))
      NameSpecUtils.testMap(Name("Alice")) should beEqualTo(None)
    }

  }

}
