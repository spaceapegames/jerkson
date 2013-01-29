package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import org.junit.{ Ignore, Test }
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class DefaultCollectionSupportSpec extends ShouldMatchers {
  @Test def `A Range generates a JSON object` {
    generate(Range.inclusive(1, 4, 3)) should equal("""{"start":1,"end":4,"step":3,"inclusive":true}""")
  }

  @Test def `A Range generates a JSON object without the inclusive field if it's exclusive` {
    generate(Range(1, 4, 3)) should equal("""{"start":1,"end":4,"step":3}""")
  }

  @Test def `A Range generates a JSON object without the step field if it's 1` {
    generate(Range(1, 4)) should equal("""{"start":1,"end":4}""")
  }

  @Test def `A Range is parsable from a JSON object` {
    parse[Range]("""{"start":1,"end":4,"step":3,"inclusive":true}""") should equal(Range.inclusive(1, 4, 3))
  }

  @Test def `A Range is parsable from a JSON object without the inclusive field` {
    parse[Range]("""{"start":1,"end":4,"step":3}""") should equal(Range(1, 4, 3))
  }

  @Test def `A Range is parsable from a JSON object without the step field` {
    parse[Range]("""{"start":1,"end":4}""") should equal(Range(1, 4))
  }

  @Test def `A Range is not parsable from a JSON object without the required fields` {
    val thrown = evaluating {
      parse[Range]("""{"start":1}""")
    } should produce[ParsingException]
    thrown.getMessage() should equal("""Invalid JSON. Needed [start, end, <step>, <inclusive>], but found [start].""")
  }

  @Ignore @Test def `A Pair[Int] generates a two-element JSON array of ints` {
    // TODO: 5/31/11 <coda> -- fix Pair serialization
    generate(Pair(1, 2)) should equal("[1,2]")
  }

  @Ignore @Test def `A Pair[Int] is parsable from a two-element JSON array of ints` {
    // TODO: 5/31/11 <coda> -- fix Pair deserialization
    parse[Pair[Int, Int]]("[1,2]") should equal(Pair(1, 2))
  }

  @Ignore @Test def `A Triple[Int] generates a three-element JSON array of ints` {
    // TODO: 5/31/11 <coda> -- fix Triple serialization
    generate(Triple(1, 2, 3)) should equal("[1,2,3]")
  }

  @Ignore @Test def `A Triple[Int] is parsable from a three-element JSON array of ints` {
    // TODO: 5/31/11 <coda> -- fix Triple deserialization
    parse[Triple[Int, Int, Int]]("[1,2,3]") should equal(Triple(1, 2, 3))
  }

  @Ignore @Test def `A four-tuple generates a four-element JSON array` {
    // TODO: 5/31/11 <coda> -- fix Tuple4 serialization
    generate((1, "2", 3, "4")) should equal("[1,\"2\",3,\"4\"]")
  }

  @Ignore @Test def `A four-tuple is parsable from a three-element JSON array of ints` {
    // TODO: 5/31/11 <coda> -- fix Tuple4 deserialization
    parse[(Int, String, Int, String)]("[1,\"2\",3,\"4\"]") should equal((1, "2", 3, "4"))
  }

  // TODO: 6/1/11 <coda> -- add support for all Tuple1->TupleBillionty types

  @Test def `A Seq[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A Seq[Int] is parsable from a JSON array of ints` {
    parse[Seq[Int]]("[1,2,3]") should equal(Seq(1, 2, 3))
  }

  @Test def `A Seq[Int] is parsable from an empty JSON array` {
    parse[Seq[Int]]("[]") should equal(Seq.empty[Int])
  }

  @Test def `A List[Int] generates a JSON array of ints` {
    generate(List(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A List[Int] is parsable from a JSON array of ints` {
    parse[List[Int]]("[1,2,3]") should equal(List(1, 2, 3))
  }

  @Test def `A List[Int] is parsable from an empty JSON array` {
    parse[List[Int]]("[]") should equal(List.empty[Int])
  }

  @Test def `An IndexedSeq[Int] generates a JSON array of ints` {
    generate(IndexedSeq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `An IndexedSeq[Int] is parsable from a JSON array of ints` {
    parse[IndexedSeq[Int]]("[1,2,3]") should equal(IndexedSeq(1, 2, 3))
  }

  @Test def `An IndexedSeq[Int] is parsable from an empty JSON array` {
    parse[IndexedSeq[Int]]("[]") should equal(IndexedSeq.empty[Int])
  }

  @Test def `A Vector[Int] generates a JSON array of ints` {
    generate(Vector(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A Vector[Int] is parsable from a JSON array of ints` {
    parse[Vector[Int]]("[1,2,3]") should equal(Vector(1, 2, 3))
  }

  @Test def `A Vector[Int] is parsable from an empty JSON array` {
    parse[Vector[Int]]("[]") should equal(Vector.empty[Int])
  }

  @Test def `A Set[Int] generates a JSON array of ints` {
    generate(Set(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A Set[Int] is parsable from a JSON array of ints` {
    parse[Set[Int]]("[1,2,3]") should equal(Set(1, 2, 3))
  }

  @Test def `A Set[Int] is parsable from an empty JSON array` {
    parse[Set[Int]]("[]") should equal(Set.empty[Int])
  }

  @Test def `A Map[String, Int] generates a JSON object with int field values` {
    generate(Map("one" -> 1, "two" -> 2)) should equal("""{"one":1,"two":2}""")
  }

  @Test def `A Map[String, Int] is parsable from a JSON object with int field values` {
    parse[Map[String, Int]]("""{"one":1,"two":2}""") should equal(Map("one" -> 1, "two" -> 2))
  }

  @Test def `A Map[String, Int] is parsable from an empty JSON object` {
    parse[Map[String, Int]]("{}") should equal(Map.empty[String, Int])
  }

  @Test def `A Map[String, Any] generates a JSON object with mixed field values` {
    generate(Map("one" -> 1, "two" -> "2")) should equal("""{"one":1,"two":"2"}""")
  }

  @Test def `A Map[String, Any] is parsable from a JSON object with mixed field values` {
    parse[Map[String, Any]]("""{"one":1,"two":"2"}""") should equal(Map[String, Any]("one" -> 1, "two" -> "2"))
  }

  @Test def `A Map[String, Any] is parsable from an empty JSON object` {
    parse[Map[String, Any]]("{}") should equal(Map.empty[String, Any])
  }

  @Test def `A Stream[Int] generates a JSON array` {
    generate(Stream(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A Stream[Int] is parsable from a JSON array of ints` {
    parse[Stream[Int]]("[1,2,3]") should equal(Stream(1, 2, 3))
  }

  @Test def `A Stream[Int] is parsable from an empty JSON array` {
    parse[Stream[Int]]("[]") should equal(Stream.empty[Int])
  }

  @Test def `An Iterator[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).iterator) should equal("[1,2,3]")
  }

  @Test def `An Iterator[Int] is parsable from a JSON array of ints` {
    parse[Iterator[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `An Iterator[Int] is parsable from an empty JSON array` {
    parse[Iterator[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A Traversable[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).toTraversable) should equal("[1,2,3]")
  }

  @Test def `A Traversable[Int] is parsable from a JSON array of ints` {
    parse[Traversable[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A Traversable[Int] is parsable from an empty JSON array` {
    parse[Traversable[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A BufferedIterator[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).iterator.buffered) should equal("[1,2,3]")
  }

  @Test def `A BufferedIterator[Int] is parsable from a JSON array of ints` {
    parse[BufferedIterator[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A BufferedIterator[Int] is parsable from an empty JSON array` {
    parse[BufferedIterator[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `An Iterable[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).toIterable) should equal("[1,2,3]")
  }

  @Test def `An Iterable[Int] is parsable from a JSON array of ints` {
    parse[Iterable[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `An Iterable[Int] is parsable from an empty JSON array` {
    parse[Iterable[Int]]("[]").toList should equal(List.empty[Int])
  }
}
