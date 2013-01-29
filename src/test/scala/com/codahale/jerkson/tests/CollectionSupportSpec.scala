package com.codahale.jerkson.tests

import scala.collection._
import com.codahale.jerkson.Json._
import org.junit.{ Ignore, Test }
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class CollectionSupportSpec extends ShouldMatchers {
  @Test def `A collection.BitSet generates a JSON array of ints` {
    generate(BitSet(1)) should equal("[1]")
  }

  @Test def `A collection.BitSet is parsable from a JSON array of ints` {
    parse[BitSet]("[1,2,3]") should equal(BitSet(1, 2, 3))
  }

  @Test def `A collection.Iterator[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).iterator) should equal("[1,2,3]")
  }

  @Test def `A collection.Iterator[Int] is parsable from a JSON array of ints` {
    parse[Iterator[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A collection.Iterator[Int] is parsable from an empty JSON array` {
    parse[Iterator[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A collection.Traversable[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).toTraversable) should equal("[1,2,3]")
  }

  @Test def `A collection.Traversable[Int] is parsable from a JSON array of ints` {
    parse[Traversable[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A collection.Traversable[Int] is parsable from an empty JSON array` {
    parse[Traversable[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A collection.BufferedIterator[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).iterator.buffered) should equal("[1,2,3]")
  }

  @Test def `A collection.BufferedIterator[Int] is parsable from a JSON array of ints` {
    parse[BufferedIterator[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A collection.BufferedIterator[Int] is parsable from an empty JSON array` {
    parse[BufferedIterator[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A collection.Iterable[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3).toIterable) should equal("[1,2,3]")
  }

  @Test def `A collection.Iterable[Int] is parsable from a JSON array of ints` {
    parse[Iterable[Int]]("[1,2,3]").toList should equal(List(1, 2, 3))
  }

  @Test def `A collection.Iterable[Int] is parsable from an empty JSON array` {
    parse[Iterable[Int]]("[]").toList should equal(List.empty[Int])
  }

  @Test def `A collection.Set[Int] generates a JSON array of ints` {
    generate(Set(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A collection.Set[Int] is parsable from a JSON array of ints` {
    parse[Set[Int]]("[1,2,3]") should equal(Set(1, 2, 3))
  }

  @Test def `A collection.Set[Int] is parsable from an empty JSON array` {
    parse[Set[Int]]("[]") should equal(Set.empty[Int])
  }

  @Test def `A collection.Map[String, Int] generates a JSON object with int field values` {
    generate(Map("one" -> 1, "two" -> 2)) should equal("""{"one":1,"two":2}""")
  }

  @Test def `A collection.Map[String, Int] is parsable from a JSON object with int field values` {
    parse[Map[String, Int]]("""{"one":1,"two":2}""") should equal(Map("one" -> 1, "two" -> 2))
  }

  @Test def `A collection.Map[String, Int] is parsable from an empty JSON object` {
    parse[Map[String, Int]]("{}") should equal(Map.empty[String, Int])
  }

  @Test def `A collection.IndexedSeq[Int] generates a JSON array of ints` {
    generate(IndexedSeq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A collection.IndexedSeq[Int] is parsable from a JSON array of ints` {
    parse[IndexedSeq[Int]]("[1,2,3]") should equal(IndexedSeq(1, 2, 3))
  }

  @Test def `A collection.IndexedSeq[Int] is parsable from an empty JSON array` {
    parse[IndexedSeq[Int]]("[]") should equal(IndexedSeq.empty)
  }

  @Test def `A collection.Seq[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A collection.Seq[Int] is parsable from a JSON array of ints` {
    parse[Seq[Int]]("[1,2,3]") should equal(Seq(1, 2, 3))
  }

  @Test def `A collection.Seq[Int] is parsable from an empty JSON array` {
    parse[Seq[Int]]("[]") should equal(Seq.empty[Int])
  }

  @Test def `A collection.SortedMap[String, Int] generates a JSON object with int field values` {
    generate(SortedMap("one" -> 1, "two" -> 2)) should equal("""{"one":1,"two":2}""")
  }

  // TODO: 6/1/11 <coda> -- figure out how to deserialize SortedMap instances

  /**
   * I think all this would take is a mapping from Class[_] to Ordering, which
   * would need to have hard-coded the various primitive types, and then add
   * support for Ordered and Comparable classes. Once we have the Ordering,
   * we can pass it in manually to a builder.
   */

  @Ignore @Test def `A collection.SortedMap[String, Int] is parsable from a JSON object with int field values` {
    parse[SortedMap[String, Int]]("""{"one":1,"two":2}""") should equal(SortedMap("one" -> 1, "two" -> 2))
  }

  @Ignore @Test def `A collection.SortedMap[String, Int] is parsable from an empty JSON object` {
    parse[SortedMap[String, Int]]("{}") should equal(SortedMap.empty[String, Int])
  }

  @Test def `A collection.SortedSet[Int] generates a JSON array of ints` {
    generate(SortedSet(1, 2, 3)) should equal("[1,2,3]")
  }

  // TODO: 6/1/11 <coda> -- figure out how to deserialize SortedMap instances

  /**
   * I think all this would take is a mapping from Class[_] to Ordering, which
   * would need to have hard-coded the various primitive types, and then add
   * support for Ordered and Comparable classes. Once we have the Ordering,
   * we can pass it in manually to a builder.
   */

  @Ignore @Test def `A collection.SortedSet[Int] is parsable from a JSON array of ints` {
    parse[SortedSet[Int]]("[1,2,3]") should equal(SortedSet(1, 2, 3))

  }

  @Ignore @Test def `A collection.SortedSet[Int] is parsable from an empty JSON array` {
    parse[SortedSet[Int]]("[]") should equal(SortedSet.empty[Int])
  }
}
