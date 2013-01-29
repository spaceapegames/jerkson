package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import java.io.ByteArrayInputStream
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class EdgeCaseSpec extends ShouldMatchers {
  @Test def `Deserializing lists doesn't cache Seq builders` {
    parse[List[Int]]("[1,2,3,4]") should equal(List(1, 2, 3, 4))
    parse[List[Int]]("[1,2,3,4]") should equal(List(1, 2, 3, 4))
  }

  @Test def `Parsing a JSON array of ints with nulls should be readable as a List[Option[Int]]` {
    parse[List[Option[Int]]]("[1,2,null,4]") should equal(List(Some(1), Some(2), None, Some(4)))
  }

  @Test def `Deserializing maps doesn't cache Map builders` {
    parse[Map[String, Int]](""" {"one":1, "two": 2} """) should equal(Map("one" -> 1, "two" -> 2))
    parse[Map[String, Int]](""" {"one":1, "two": 2} """) should equal(Map("one" -> 1, "two" -> 2))
  }

  @Test def `Parsing malformed JSON should throw a ParsingException with an informative message` {
    val thrown = evaluating {
      parse[Boolean]("jjf8;09")
    } should produce[ParsingException]
    thrown.getMessage() should equal(
      "Malformed JSON. Unexpected character ('j' (code 106)): expected a " +
        "valid value (number, String, array, object, 'true', 'false' " +
        "or 'null') at character offset 0.")

    val thrown2 = evaluating {
      parse[CaseClass]("{\"ye\":1")
    } should produce[ParsingException]
    thrown2.getMessage() should equal(
      "Malformed JSON. Unexpected end-of-input: expected close marker for " +
        "OBJECT at character offset 20.")
  }

  @Test def `Parsing invalid JSON should throw a ParsingException with an informative message` {
    val thrown = evaluating {
      parse[CaseClass]("900")
    } should produce[ParsingException]
    thrown.getMessage() should include regex (
      ("""Can not deserialize instance of com.codahale.jerkson.tests.CaseClass out of VALUE_NUMBER_INT token\n""" +
        """ at \[Source: java.io.StringReader@[0-9a-f]+; line: 1, column: 1\]""").r)

    val thrown2 = evaluating {
      parse[CaseClass]("{\"woo\": 1}")
    } should produce[ParsingException]
    thrown2.getMessage() should be("Invalid JSON. Needed [id, name], but found [woo].")
  }

  @Test def `Parsing an empty document should throw a ParsingException with an informative message` {
    val input = new ByteArrayInputStream(Array.empty)
    val thrown = evaluating {
      parse[CaseClass](input)
    } should produce[ParsingException]
    thrown.getMessage() should startWith("""No content to map due to end-of-input""")
  }
}
