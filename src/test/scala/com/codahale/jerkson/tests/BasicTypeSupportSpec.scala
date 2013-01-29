package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.fasterxml.jackson.databind.node.IntNode
import com.fasterxml.jackson.databind.JsonNode
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

class BasicTypeSupportSpec extends ShouldMatchers {
    @Test def `A Byte generates a JSON int` {
      generate(15.toByte) should equal ("15")
    }

    @Test def `A Byte is parsable from a JSON int` {
      parse[Byte]("15") should equal (15)
    }

    @Test def `A Short generates a JSON int` {
      generate(15.toShort) should equal ("15")
    }

    @Test def `A Short is parsable from a JSON int` {
      parse[Short]("15") should equal (15)
    }

    @Test def `An Int generates a JSON int` {
      generate(15) should equal ("15")
    }

    @Test def `An Int is parsable from a JSON int` {
      parse[Int]("15") should equal (15)
    }

    @Test def `A Long generates a JSON int` {
      generate(15L) should equal ("15")
    }

    @Test def `A Long is parsable from a JSON int` {
      parse[Long]("15") should equal (15L)
    }

    @Test def `A BigInt generates a JSON int` {
      generate(BigInt(15)) should equal ("15")
    }

    @Test def `A BigInt is parsable from a JSON int` {
      parse[BigInt]("15") should equal (BigInt(15))
    }

    @Test def `A BigInt is parsable from a JSON string` {
      parse[BigInt]("\"15\"") should equal (BigInt(15))
    }

    @Test def `A Float generates a JSON float` {
      generate(15.1F) should equal ("15.1")
    }

    @Test def `A Float is parsable from a JSON float` {
      parse[Float]("15.1") should equal (15.1F)
    }

    @Test def `A Double generates a JSON float` {
      generate(15.1) should equal ("15.1")
    }

    @Test def `A Double is parsable from a JSON float` {
      parse[Double]("15.1") should equal (15.1D)
    }

    @Test def `A BigDecimal generates a JSON float` {
      generate(BigDecimal(15.5)) should equal ("15.5")
    }

    @Test def `A BigDecimal is parsable from a JSON float` {
      parse[BigDecimal]("15.5") should equal (BigDecimal(15.5))
    }

    @Test def `A BigDecimal is parsable from a JSON int` {
      parse[BigDecimal]("15") should equal (BigDecimal(15.0))
    }

    @Test def `A String generates a JSON string` {
      generate("woo") should equal ("\"woo\"")
    }

    @Test def `A String is parsable from a JSON string` {
      parse[String]("\"woo\"") should equal ("woo")
    }

    @Test def `A StringBuilder generates a JSON string` {
      generate(new StringBuilder("foo")) should equal ("\"foo\"")
    }

    @Test def `A StringBuilder is parsable from a JSON string` {
      parse[StringBuilder]("\"foo\"").toString() should equal ("foo")
    }

    @Test def `A null Object generates a JSON null` {
      generate[Object](null) should equal ("null")
    }

    @Test def `A null Object is parsable from a JSON null` {
      parse[Object]("null") should equal (null)
    }

    @Test def `A Boolean generates a JSON true` {
      generate(true) should equal ("true")
    }

    @Test def `A Boolean generates a JSON false` {
      generate(false) should equal ("false")
    }

    @Test def `A Boolean is parsable from a JSON true` {
      parse[Boolean]("true") should equal (true)
    }

    @Test def `A Boolean is parsable from a JSON false` {
      parse[Boolean]("false") should equal (false)
    }

    @Test def `A Some[Int] generates a JSON int` {
      generate(Some(12)) should equal ("12")
    }

    @Test def `A Some[Int] is parsable from a JSON int as an Option[Int]` {
      parse[Option[Int]]("12") should equal (Some(12))
    }

    @Test def `A None generates a JSON null` {
      generate(None) should equal ("null")
    }

    @Test def `A None is parsable from a JSON null as an Option[Int]` {
      parse[Option[Int]]("null") should equal (None)
    }

    @Test def `A Left[String] generates a JSON string` {
      generate(Left("woo")) should equal ("\"woo\"")
    }

    @Test def `A Left[String] is parsable from a JSON string as an Either[String, Int]` {
      parse[Either[String, Int]]("\"woo\"") should equal (Left("woo"))
    }

    @Test def `A Right[String] generates a JSON string` {
      generate(Right("woo")) should equal ("\"woo\"")
    }

    @Test def `A Right[String] is parsable from a JSON string as an Either[Int, String]` {
      parse[Either[Int, String]]("\"woo\"") should equal (Right("woo"))
    }

    @Test def `A JsonNode generates whatever the JsonNode is` {
      generate(new IntNode(2)) should equal ("2")
    }

    @Test def `A JsonNode is parsable from a JSON AST node` {
      parse[JsonNode]("2") should equal (new IntNode(2))
    }

    @Test def `A JsonNode is parsable from a JSON AST node as a specific type` {
      parse[IntNode]("2") should equal (new IntNode(2))
    }

    @Test def `A JsonNode is itself parsable` {
      parse[Int](new IntNode(2)) should equal (2)
    }

    @Test def `An Array[Int] generates a JSON array of ints` {
      generate(Array(1, 2, 3)) should equal ("[1,2,3]")
    }

    @Test def `An Array[Int] is parsable from a JSON array of ints` {
      parse[Array[Int]]("[1,2,3]").toList should equal (List(1, 2, 3))
    }

    @Test def `An Array[Int] is parsable from an empty JSON array` {
      parse[Array[Int]]("[]").toList should equal (List.empty)
    }
}
