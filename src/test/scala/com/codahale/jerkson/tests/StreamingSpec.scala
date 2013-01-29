package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import java.io.ByteArrayInputStream
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class StreamingSpec extends ShouldMatchers {
  val json = """[
      {"id":1, "name": "Coda"},
      {"id":2, "name": "Niki"},
      {"id":3, "name": "Biscuit"},
      {"id":4, "name": "Louie"}
    ]"""

  @Test def `Parsing a stream of objects returns an iterator of stream elements` {
    stream[CaseClass](new ByteArrayInputStream(json.getBytes)).toList should equal (CaseClass(1, "Coda") :: CaseClass(2, "Niki") ::
      CaseClass(3, "Biscuit") :: CaseClass(4, "Louie") :: Nil)
  }
}
