package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import com.fasterxml.jackson.databind.node.IntNode
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class CaseClassSupportSpec extends ShouldMatchers {
  @Test def `A basic case classgenerates a JSON object with matching field values` {
    generate(CaseClass(1, "Coda")) should equal("""{"id":1,"name":"Coda"}""")
  }

  @Test def `A basic case class is parsable from a JSON object with corresponding fields` {
    parse[CaseClass]("""{"id":1,"name":"Coda"}""") should equal(CaseClass(1, "Coda"))
  }

  @Test def `A basic case class is parsable from a JSON object with extra fields` {
    parse[CaseClass]("""{"id":1,"name":"Coda","derp":100}""") should equal(CaseClass(1, "Coda"))
  }

  @Test def `A basic case class is not parsable from an incomplete JSON object` {
    val thrown = evaluating {
      parse[CaseClass]("""{"id":1}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("""Invalid JSON. Needed [id, name], but found [id].""")
  }

  @Test def `A case class with lazy fields generates a JSON object with those fields evaluated` {
    generate(CaseClassWithLazyVal(1)) should equal("""{"id":1,"woo":"yeah"}""")
  }

  @Test def `A case class with lazy fields is parsable from a JSON object without those fields` {
    parse[CaseClassWithLazyVal]("""{"id":1}""") should equal(CaseClassWithLazyVal(1))
  }

  @Test def `A case class with lazy fields is not parsable from an incomplete JSON object` {
    val thrown = evaluating {
      parse[CaseClassWithLazyVal]("""{}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("""Invalid JSON. Needed [id], but found [].""")
  }

  @Test def `A case class with ignored members generates a JSON object without those fields` {
    generate(CaseClassWithIgnoredField(1)) should equal("""{"id":1}""")
    generate(CaseClassWithIgnoredFields(1)) should equal("""{"id":1}""")
  }

  @Test def `A case class with ignored members is parsable from a JSON object without those fields` {
    parse[CaseClassWithIgnoredField]("""{"id":1}""") should equal(CaseClassWithIgnoredField(1))
    parse[CaseClassWithIgnoredFields]("""{"id":1}""") should equal(CaseClassWithIgnoredFields(1))
  }

  @Test def `A case class with ignored members is not parsable from an incomplete JSON object` {
    val thrown = evaluating {
      parse[CaseClassWithIgnoredField]("""{}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("""Invalid JSON. Needed [id], but found [].""")

    val thrown2 = evaluating {
      parse[CaseClassWithIgnoredFields]("""{}""")
    } should produce[ParsingException]
    thrown2.getMessage() should equal("""Invalid JSON. Needed [id], but found [].""")
  }

  @Test def `A case class with transient members generates a JSON object without those fields` {
    generate(CaseClassWithTransientField(1)) should equal("""{"id":1}""")
  }

  @Test def `A case class with transient members is parsable from a JSON object without those fields` {
    parse[CaseClassWithTransientField]("""{"id":1}""") should equal(CaseClassWithTransientField(1))
  }

  @Test def `A case class with transient members is not parsable from an incomplete JSON object` {
    val thrown = evaluating {
      parse[CaseClassWithTransientField]("""{}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("""Invalid JSON. Needed [id], but found [].""")
  }

  @Test def `A case class with an overloaded field generates a JSON object with the nullary version of that field` {
    generate(CaseClassWithOverloadedField(1)) should equal("""{"id":1}""")
  }

  @Test def `A case class with an Option[String] member generates a field if the member is Some` {
    generate(CaseClassWithOption(Some("what"))) should equal("""{"value":"what"}""")
  }

  @Test def `A case class with an Option[String] member is parsable from a JSON object with that field` {
    parse[CaseClassWithOption]("""{"value":"what"}""") should equal(CaseClassWithOption(Some("what")))
  }

  @Test def `A case class with an Option[String] member doesn't generate a field if the member is None` {
    generate(CaseClassWithOption(None)) should equal("""{}""")
  }

  @Test def `A case class with an Option[String] member is parsable from a JSON object without that field` {
    parse[CaseClassWithOption]("""{}""") should equal(CaseClassWithOption(None))
  }

  @Test def `A case class with an Option[String] member is parsable from a JSON object with a null value for that field` {
    parse[CaseClassWithOption]("""{"value":null}""") should equal(CaseClassWithOption(None))
  }

  @Test def `A case class with a JsonNode member generates a field of the given type` {
    generate(CaseClassWithJsonNode(new IntNode(2))) should equal("""{"value":2}""")
  }

  val json = """
               {
                 "map": {
                   "one": "two"
                 },
                 "set": [1, 2, 3],
                 "string": "woo",
                 "list": [4, 5, 6],
                 "seq": [7, 8, 9],
                 "sequence": [10, 11, 12],
                 "collection": [13, 14, 15],
                 "indexedSeq": [16, 17, 18],
                 "randomAccessSeq": [19, 20, 21],
                 "vector": [22, 23, 24],
                 "bigDecimal": 12.0,
                 "bigInt": 13,
                 "int": 1,
                 "long": 2,
                 "char": "x",
                 "bool": false,
                 "short": 14,
                 "byte": 15,
                 "float": 34.5,
                 "double": 44.9,
                 "any": true,
                 "anyRef": "wah",
                 "intMap": {
                   "1": "1"
                 },
                 "longMap": {
                   "2": 2
                 }
               }
               """

  @Test def `A case class with members of all ScalaSig types is parsable from a JSON object with those fields` {
    parse[CaseClassWithAllTypes](json) should equal(
      CaseClassWithAllTypes(
        map = Map("one" -> "two"),
        set = Set(1, 2, 3),
        string = "woo",
        list = List(4, 5, 6),
        seq = Seq(7, 8, 9),
        indexedSeq = IndexedSeq(16, 17, 18),
        vector = Vector(22, 23, 24),
        bigDecimal = BigDecimal("12.0"),
        bigInt = BigInt("13"),
        int = 1,
        long = 2L,
        char = 'x',
        bool = false,
        short = 14,
        byte = 15,
        float = 34.5f,
        double = 44.9d,
        any = true,
        anyRef = "wah",
        intMap = Map(1 -> 1),
        longMap = Map(2L -> 2L)))
  }

  @Test def `A case class nested inside of an object is parsable from a JSON object` {
    parse[OuterObject.NestedCaseClass]("""{"id": 1}""") should equal(OuterObject.NestedCaseClass(1))
  }

  @Test def `A case class nested inside of an object nested inside of an object is parsable from a JSON object` {
    parse[OuterObject.InnerObject.SuperNestedCaseClass]("""{"id": 1}""") should equal(OuterObject.InnerObject.SuperNestedCaseClass(1))
  }

  @Test def `A case class with two constructors is parsable from a JSON object with the same parameters as the case accessor` {
    parse[CaseClassWithTwoConstructors]("""{"id":1,"name":"Bert"}""") should equal(CaseClassWithTwoConstructors(1, "Bert"))
  }

  @Test def `A case class with two constructors is parsable from a JSON object which works with the second constructor` {
    val thrown = evaluating {
      parse[CaseClassWithTwoConstructors]("""{"id":1}""")
    } should produce[ParsingException]
  }

  @Test def `A case class with snake-cased fields is parsable from a snake-cased JSON object` {
    parse[CaseClassWithSnakeCase]("""{"one_thing":"yes","two_thing":"good"}""") should equal(CaseClassWithSnakeCase("yes", "good"))
  }

  @Test def `A case class with snake-cased fields generates a snake-cased JSON object` {
    generate(CaseClassWithSnakeCase("yes", "good")) should equal("""{"one_thing":"yes","two_thing":"good"}""")
  }

  @Test def `A case class with snake-cased fields throws errors with the snake-cased field names present` {
    val thrown = evaluating {
      parse[CaseClassWithSnakeCase]("""{"one_thing":"yes"}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("Invalid JSON. Needed [one_thing, two_thing], but found [one_thing].")
  }

  @Test def `A case class with array members is parsable from a JSON object` {
    val c = parse[CaseClassWithArrays]("""{"one":"1","two":["a","b","c"],"three":[1,2,3]}""")

    c.one should equal("1")
    c.two should equal(Array("a", "b", "c"))
    c.three should equal(Array(1, 2, 3))
  }

  @Test def `A case class with array members generates a JSON object` {
    generate(CaseClassWithArrays("1", Array("a", "b", "c"), Array(1, 2, 3))) should equal(
      """{"one":"1","two":["a","b","c"],"three":[1,2,3]}""")
  }
}
