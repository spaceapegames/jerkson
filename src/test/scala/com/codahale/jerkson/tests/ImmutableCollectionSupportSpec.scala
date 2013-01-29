package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import scala.collection.immutable._
import com.codahale.jerkson.ParsingException
import org.junit.{ Ignore, Test }
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class ImmutableCollectionSupportSpec extends ShouldMatchers {
  @Test def `An immutable.Seq[Int] generates a JSON array of ints` {
    generate(Seq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `An immutable.Seq[Int] is parsable from a JSON array of ints` {
    parse[Seq[Int]]("[1,2,3]") should equal(Seq(1, 2, 3))
  }

  @Test def `An immutable.Seq[Int] is parsable from an empty JSON array` {
    parse[Seq[Int]]("[]") should equal(Seq.empty[Int])
  }

  @Test def `An immutable.List[Int] generates a JSON array of ints` {
    generate(List(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `An immutable.List[Int] is parsable from a JSON array of ints` {
    parse[List[Int]]("[1,2,3]") should equal(List(1, 2, 3))
  }

  @Test def `An immutable.List[Int] is parsable from an empty JSON array` {
    parse[List[Int]]("[]") should equal(List.empty[Int])
  }

  @Test def `An immutable.IndexedSeq[Int] generates a JSON array of ints` {
    generate(IndexedSeq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `An immutable.IndexedSeq[Int] is parsable from a JSON array of ints` {
    parse[IndexedSeq[Int]]("[1,2,3]") should equal(IndexedSeq(1, 2, 3))
  }

  @Test def `An immutable.IndexedSeq[Int] is parsable from an empty JSON array` {
    parse[IndexedSeq[Int]]("[]") should equal(IndexedSeq.empty[Int])
  }

  @Test def `An immutable.TreeSet[Int] generates a JSON array` {
    generate(TreeSet(1)) should equal("[1]")
  }

  // TODO: 6/1/11 <coda> -- figure out how to deserialize TreeSet instances

  /**
   * I think all this would take is a mapping from Class[_] to Ordering, which
   * would need to have hard-coded the various primitive types, and then add
   * support for Ordered and Comparable classes. Once we have the Ordering,
   * we can pass it in manually to a builder.
   */

  @Ignore @Test def `An immutable.TreeSet[Int] is parsable from a JSON array of ints` {
    parse[TreeSet[Int]]("[1,2,3]") should equal(TreeSet(1, 2, 3))
  }

  @Ignore @Test def `An immutable.TreeSet[Int] is parsable from an empty JSON array` {
    parse[TreeSet[Int]]("[]") should equal(TreeSet.empty[Int])
  }

  @Test def `An immutable.HashSet[Int] generates a JSON array` {
    generate(HashSet(1)) should equal("[1]")
  }

  @Test def `An immutable.HashSet[Int] is parsable from a JSON array of ints` {
    parse[HashSet[Int]]("[1,2,3]") should equal(HashSet(1, 2, 3))
  }

  @Test def `An immutable.HashSet[Int] is parsable from an empty JSON array` {
    parse[HashSet[Int]]("[]") should equal(HashSet.empty[Int])
  }

  @Test def `An immutable.BitSet generates a JSON array` {
    generate(BitSet(1)) should equal("[1]")
  }

  @Test def `An immutable.BitSet is parsable from a JSON array of ints` {
    parse[BitSet]("[1,2,3]") should equal(BitSet(1, 2, 3))
  }

  @Test def `An immutable.BitSet is parsable from an empty JSON array` {
    parse[BitSet]("[]") should equal(BitSet.empty)
  }

  @Test def `An immutable.TreeMap[String, Int] generates a JSON object` {
    generate(TreeMap("one" -> 1)) should equal("""{"one":1}""")
  }

  // TODO: 6/1/11 <coda> -- figure out how to deserialize TreeMap instances

  /**
   * I think all this would take is a mapping from Class[_] to Ordering, which
   * would need to have hard-coded the various primitive types, and then add
   * support for Ordered and Comparable classes. Once we have the Ordering,
   * we can pass it in manually to a builder.
   */

  @Ignore @Test def `An immutable.TreeMap[String, Int] is parsable from a JSON object with int field values` {
    parse[TreeMap[String, Int]]("""{"one":1}""") should equal(TreeMap("one" -> 1))
  }

  @Ignore @Test def `An immutable.TreeMap[String, Int] is parsable from an empty JSON object` {
    parse[TreeMap[String, Int]]("{}") should equal(TreeMap.empty[String, Int])
  }

  @Test def `An immutable.HashMap[String, Int] generates a JSON object` {
    generate(HashMap("one" -> 1)) should equal("""{"one":1}""")
  }

  @Test def `An immutable.HashMap[String, Int] is parsable from a JSON object with int field values` {
    parse[HashMap[String, Int]]("""{"one":1}""") should equal(HashMap("one" -> 1))
  }

  @Test def `An immutable.HashMap[String, Int] is parsable from an empty JSON object` {
    parse[HashMap[String, Int]]("{}") should equal(HashMap.empty[String, Int])
  }

  @Test def `An immutable.HashMap[String, Any] generates a JSON object` {
    generate(HashMap[String, Any]("one" -> 1)) should equal("""{"one":1}""")
  }

  @Test def `An immutable.HashMap[String, Any] is parsable from a JSON object with int field values` {
    parse[HashMap[String, Any]]("""{"one":1}""") should equal(HashMap("one" -> 1))
  }

  @Test def `An immutable.HashMap[String, Any] is parsable from an empty JSON object` {
    parse[HashMap[String, Any]]("{}") should equal(HashMap.empty[String, Any])
  }

  @Test def `An immutable.HashMap[String, Any] is not parsable from an empty JSON object in a JSON array` {
    val thrown = evaluating {
      parse[HashMap[String, Any]]("[{}]")
    } should produce[ParsingException]
  }

  @Test def `An immutable.Map[Int, String] generates a JSON object` {
    generate(Map(1 -> "one")) should equal("""{"1":"one"}""")
  }

  @Test def `An immutable.Map[Int, String] is parsable from a JSON object with decimal field names and string field values` {
    parse[Map[Int, String]]("""{"1":"one"}""") should equal(Map(1 -> "one"))
  }

  @Test def `An immutable.Map[Int, String] is not parsable from a JSON object with non-decimal field names` {
    val thrown = evaluating {
      parse[Map[Int, String]]("""{"one":"one"}""")
    } should produce[ParsingException]
  }

  @Test def `An immutable.Map[Int, String] is parsable from an empty JSON object` {
    parse[Map[Int, String]]("{}") should equal(Map.empty[Int, String])
  }

  @Test def `An immutable.Map[Int, Any] is not parsable from an empty JSON object in a JSON array` {
    val thrown = evaluating {
      parse[Map[Int, Any]]("[{}]")
    } should produce[ParsingException]
  }

  @Test def `An immutable.IntMap[Any] is not parsable from an empty JSON object in a JSON array` {
    evaluating {
      parse[IntMap[Any]]("[{}]")
    } should produce [ParsingException]
  }

  @Test def `An immutable.LongMap[Any] is not parsable from an empty JSON object in a JSON array` {
    evaluating {
      parse[LongMap[Any]]("[{}]")
    } should produce[ParsingException]
  }

  @Test def `An immutable.Map[Long, Any] is not parsable from an empty JSON object in a JSON array` {
    evaluating {
      parse[Map[Long, Any]]("[{}]")
    } should produce[ParsingException]
  }

  @Test def `An immutable.Map[Long, String] generates a JSON object` {
    generate(Map(1L -> "one")) should equal("""{"1":"one"}""")
  }

  @Test def `An immutable.Map[Long, String] is parsable from a JSON object with decimal field names and string field values` {
    parse[Map[Long, String]]("""{"1":"one"}""") should equal(Map(1L -> "one"))
  }

  @Test def `An immutable.Map[Long, String] is not parsable from a JSON object with non-decimal field names` {
    evaluating {
      parse[Map[Long, String]]("""{"one":"one"}""")
    } should produce[ParsingException]
  }

  @Test def `An immutable.Map[Long, String] is parsable from an empty JSON object` {
    parse[Map[Long, String]]("{}") should equal(Map.empty[Long, String])
  }

  @Test def `An immutable.IntMap[String] generates a JSON object` {
    generate(IntMap(1 -> "one")) should equal("""{"1":"one"}""")
  }

  @Test def `An immutable.IntMap[String] is parsable from a JSON object with decimal field names and string field values` {
    parse[IntMap[String]]("""{"1":"one"}""") should equal(IntMap(1 -> "one"))
  }

  @Test def `An immutable.IntMap[String] is not parsable from a JSON object with non-decimal field names` {
    evaluating {
      parse[IntMap[String]]("""{"one":"one"}""")
    } should produce[ParsingException]
  }

  @Test def `An immutable.IntMap[String] is parsable from an empty JSON object` {
    parse[IntMap[String]]("{}") should equal(IntMap.empty[String])
  }

  @Test def `An immutable.LongMap[String] generates a JSON object` {
    generate(LongMap(1L -> "one")) should equal("""{"1":"one"}""")
  }

  @Test def `An immutable.LongMap[String] is parsable from a JSON object with int field names and string field values` {
    parse[LongMap[String]]("""{"1":"one"}""") should equal(LongMap(1L -> "one"))
  }

  @Test def `An immutable.LongMap[String] is not parsable from a JSON object with non-decimal field names` {
    evaluating {
      parse[LongMap[String]]("""{"one":"one"}""")
    } should produce[ParsingException]
  }

  @Test def `An immutable.LongMap[String] is parsable from an empty JSON object` {
    parse[LongMap[String]]("{}") should equal(LongMap.empty)
  }

  @Test def `An immutable.Queue[Int] generates a JSON array` {
    generate(Queue(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `An immutable.Queue[Int] is parsable from a JSON array of ints` {
    parse[Queue[Int]]("[1,2,3]") should equal(Queue(1, 2, 3))
  }

  @Test def `An immutable.Queue[Int] is parsable from an empty JSON array` {
    parse[Queue[Int]]("[]") should equal(Queue.empty)
  }
}
