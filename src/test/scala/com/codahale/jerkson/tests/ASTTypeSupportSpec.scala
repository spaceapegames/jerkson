package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.AST._
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec

class ASTTypeSupportSpec extends ShouldMatchers {
    @Test def `An AST.JInt generates a JSON int` {
      generate(JInt(15)) should equal ("15")
    }

    @Test def `An AST.JInt is parsable from a JSON int` {
      parse[JInt]("15") should equal (JInt(15))
    }

    @Test def `An AST.JInt is parsable from a JSON int as a JValue` {
      parse[JValue]("15") should equal (JInt(15))
    }

    @Test def `An AST.JFloat generates a JSON int` {
      generate(JFloat(15.1)) should equal ("15.1")
    }

    @Test def `An AST.JFloat is parsable from a JSON float` {
      parse[JFloat]("15.1") should equal (JFloat(15.1))
    }

    @Test def `An AST.JFloat is parsable from a JSON float as a JValue` {
      parse[JValue]("15.1") should equal (JFloat(15.1))
    }


    @Test def `An AST.JString generates a JSON string` {
      generate(JString("woo")) should equal ("\"woo\"")
    }

    @Test def `An AST.JString is parsable from a JSON string` {
      parse[JString]("\"woo\"") should equal (JString("woo"))
    }

    @Test def `An AST.JString is parsable from a JSON string as a JValue` {
      parse[JValue]("\"woo\"") should equal(JString("woo"))
    }

    @Test def `An AST.JNull generates a JSON null` {
      generate(JNull) should equal ("null")
    }

    @Test def `An AST.JNull is parsable from a JSON null` {
      parse[JNull.type]("null") should equal (JNull)
    }

    @Test def `An AST.JNull is parsable from a JSON null as a JValue` {
      parse[JValue]("null") should equal (JNull)
    }

    @Test def `An AST.JBoolean generates a JSON true` {
      generate(JBoolean(true)) should equal ("true")
    }

    @Test def `An AST.JBoolean generates a JSON false` {
      generate(JBoolean(false)) should equal ("false")
    }

    @Test def `An AST.JBoolean is parsable from a JSON true` {
      parse[JBoolean]("true") should equal (JBoolean(true))
    }

    @Test def `An AST.JBoolean is parsable from a JSON false` {
      parse[JBoolean]("false") should equal (JBoolean(false))
    }

    @Test def `An AST.JBoolean is parsable from a JSON true as a JValue` {
      parse[JValue]("true") should equal (JBoolean(true))
    }

    @Test def `An AST.JBoolean is parsable from a JSON false as a JValue` {
      parse[JValue]("false") should equal (JBoolean(false))
    }

    @Test def `An AST.JArray of JInts generates a JSON array of ints` {
      generate(JArray(List(JInt(1), JInt(2), JInt(3)))) should equal ("[1,2,3]")
    }

    @Test def `An AST.JArray of JInts is parsable from a JSON array of ints` {
      parse[JArray]("[1,2,3]") should equal (JArray(List(JInt(1), JInt(2), JInt(3))))
    }

    @Test def `An AST.JArray of JInts is parsable from a JSON array of ints as a JValue` {
      parse[JValue]("[1,2,3]") should equal (JArray(List(JInt(1), JInt(2), JInt(3))))
    }

    val obj = JObject(List(JField("id", JInt(1)), JField("name", JString("Coda"))))

    @Test def `An AST.JObject generates a JSON object with matching field values` {
      generate(obj) should equal ("""{"id":1,"name":"Coda"}""")
    }

    @Test def `An AST.JObject is parsable from a JSON object` {
      parse[JObject]("""{"id":1,"name":"Coda"}""") should equal (obj)
    }

    @Test def `An AST.JObject is parsable from a JSON object as a JValue` {
      parse[JValue]("""{"id":1,"name":"Coda"}""") should equal (obj)
    }

    @Test def `An AST.JObject is parsable from an empty JSON object` {
      parse[JObject]("""{}""") should equal (JObject(Nil))
    }
}
