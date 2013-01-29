package com.codahale.jerkson.tests

import java.net.URI
import org.junit.Test
import com.codahale.jerkson.Json._
import java.util.UUID
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class FancyTypeSupportSpec extends ShouldMatchers {
  @Test def `A URI generates a JSON string` {
    generate(new URI("http://example.com/resource?query=yes")) should equal("\"http://example.com/resource?query=yes\"")
  }

  @Test def `A URI is parsable from a JSON string` {
    parse[URI]("\"http://example.com/resource?query=yes\"") should equal(new URI("http://example.com/resource?query=yes"))
  }

  val uuid = UUID.fromString("a62047e4-bfb5-4d71-aad7-1a6b338eee63")

  @Test def `A UUID generates a JSON string` {
    generate(uuid) should equal("\"a62047e4-bfb5-4d71-aad7-1a6b338eee63\"")
  }

  @Test def `A UUID is parsable from a JSON string` {
    parse[UUID]("\"a62047e4-bfb5-4d71-aad7-1a6b338eee63\"") should equal(uuid)
  }
}
