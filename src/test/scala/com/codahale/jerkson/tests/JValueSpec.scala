package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.AST._
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class JValueSpec extends ShouldMatchers {
  @Test def `Selecting single nodes returns None with primitives` {
    (parse[JValue]("8") \ "blah") should equal(JNull)
  }

  @Test def `Selecting single nodes returns None on nonexistent fields` {
    (parse[JValue]("{\"one\": \"1\"}") \ "two") should equal(JNull)
  }

  @Test def `Selecting single nodes returns a JValue with an existing field` {
    (parse[JValue]("{\"one\": \"1\"}") \ "one") should equal(JString("1"))
  }

  @Test def `Selecting array members returns None with primitives` {
    (parse[JValue]("\"derp\"").apply(0)) should equal(JNull)
  }

  @Test def `Selecting array members returns None on out of bounds` {
    (parse[JValue]("[0, 1, 2, 3]").apply(4)) should equal(JNull)
  }

  @Test def `Selecting array members returns a JValue` {
    (parse[JValue]("[0, 1, 2, 3]").apply(2)) should equal(JInt(2))
  }

  @Test def `Deep selecting returns Nil with primitives` {
    (parse[JValue]("0.234") \\ "herp") should equal(List())
  }

  @Test def `Deep selecting returns Nil on nothing found` {
    (parse[JValue]("{\"one\": {\"two\" : \"three\"}}") \\ "four") should equal(List())
  }

  @Test def `Deep selecting returns single leaf nodes` {
    (parse[JValue]("{\"one\": {\"two\" : \"three\"}}") \\ "two") should equal(Seq(JString("three")))
  }

  @Test def `Deep selecting should return multiple leaf nodes` {
    (parse[JValue]("{\"one\": {\"two\" : \"three\"}, \"four\": {\"two\" : \"five\"}}") \\ "two") should equal(Seq(JString("three"), JString("five")))
  }
}
