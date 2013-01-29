package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import scala.collection.mutable._
import com.codahale.jerkson.ParsingException
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class MutableCollectionSupportSpec extends ShouldMatchers {
  @Test def `A mutable.ResizableArray[Int] generates a JSON array of ints` {
    generate(ResizableArray(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A mutable.ResizableArray[Int] is parsable from a JSON array of ints` {
    parse[ResizableArray[Int]]("[1,2,3]") should equal(ResizableArray(1, 2, 3))
  }

  @Test def `A mutable.ResizableArray[Int] is parsable from an empty JSON array` {
    parse[ResizableArray[Int]]("[]") should equal(ResizableArray.empty[Int])
  }

  @Test def `A mutable.ArraySeq[Int] generates a JSON array of ints` {
    generate(ArraySeq(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A mutable.ArraySeq[Int] is parsable from a JSON array of ints` {
    parse[ArraySeq[Int]]("[1,2,3]").equals (ArraySeq(1, 2, 3))
  }

  @Test def `A mutable.ArraySeq[Int] is parsable from an empty JSON array` {
    parse[ArraySeq[Int]]("[]") equals(ArraySeq.empty[Int])
  }

  private val xs = new MutableList[Int]
  xs ++= List(1, 2, 3)

  @Test def `A mutable.MutableList[Int] generates a JSON array` {
    generate(xs) should equal("[1,2,3]")
  }

  @Test def `A mutable.MutableList[Int] is parsable from a JSON array of ints` {
    parse[MutableList[Int]]("[1,2,3]") should equal(xs)
  }

  @Test def `A mutable.MutableList[Int] is parsable from an empty JSON array` {
    parse[MutableList[Int]]("[]") should equal(new MutableList[Int]())
  }

  @Test def `A mutable.Queue[Int] generates a JSON array` {
    generate(Queue(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A mutable.Queue[Int] is parsable from a JSON array of ints` {
    parse[Queue[Int]]("[1,2,3]") should equal(Queue(1, 2, 3))
  }

  @Test def `A mutable.Queue[Int] is parsable from an empty JSON array` {
    parse[Queue[Int]]("[]") should equal(new Queue[Int]())
  }

  @Test def `A mutable.ListBuffer[Int] generates a JSON array` {
    generate(ListBuffer(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A mutable.ListBuffer[Int] is parsable from a JSON array of ints` {
    parse[ListBuffer[Int]]("[1,2,3]") should equal(ListBuffer(1, 2, 3))
  }

  @Test def `A mutable.ListBuffer[Int] is parsable from an empty JSON array` {
    parse[ListBuffer[Int]]("[]") should equal(ListBuffer.empty[Int])
  }

  @Test def `A mutable.ArrayBuffer[Int] generates a JSON array` {
    generate(ArrayBuffer(1, 2, 3)) should equal("[1,2,3]")
  }

  @Test def `A mutable.ArrayBuffer[Int] is parsable from a JSON array of ints` {
    parse[ArrayBuffer[Int]]("[1,2,3]") should equal(ArrayBuffer(1, 2, 3))
  }

  @Test def `A mutable.ArrayBuffer[Int] is parsable from an empty JSON array` {
    parse[ArrayBuffer[Int]]("[]") should equal(ArrayBuffer.empty[Int])
  }

  @Test def `A mutable.BitSet generates a JSON array` {
    generate(BitSet(1)) should equal("[1]")
  }

  @Test def `A mutable.BitSet is parsable from a JSON array of ints` {
    parse[BitSet]("[1,2,3]") should equal(BitSet(1, 2, 3))
  }

  @Test def `A mutable.BitSet is parsable from an empty JSON array` {
    parse[BitSet]("[]") should equal(BitSet.empty)
  }

  @Test def `A mutable.HashSet[Int] generates a JSON array` {
    generate(HashSet(1)) should equal("[1]")
  }

  @Test def `A mutable.HashSet[Int] is parsable from a JSON array of ints` {
    parse[HashSet[Int]]("[1,2,3]") should equal(HashSet(1, 2, 3))
  }

  @Test def `A mutable.HashSet[Int] is parsable from an empty JSON array` {
    parse[HashSet[Int]]("[]") should equal(HashSet.empty[Int])
  }

  @Test def `A mutable.LinkedHashSet[Int] generates a JSON array` {
    generate(LinkedHashSet(1)) should equal("[1]")
  }

  @Test def `A mutable.LinkedHashSet[Int] is parsable from a JSON array of ints` {
    parse[LinkedHashSet[Int]]("[1,2,3]") should equal(LinkedHashSet(1, 2, 3))
  }

  @Test def `A mutable.LinkedHashSet[Int] is parsable from an empty JSON array` {
    parse[LinkedHashSet[Int]]("[]") should equal(LinkedHashSet.empty[Int])
  }

  @Test def `A mutable.Map[String, Int] generates a JSON object` {
    generate(Map("one" -> 1)) should equal("""{"one":1}""")
  }

  @Test def `A mutable.Map[String, Int] is parsable from a JSON object with int field values` {
    parse[Map[String, Int]]("""{"one":1}""") should equal(Map("one" -> 1))
  }

  @Test def `A mutable.Map[String, Int] is parsable from an empty JSON object` {
    parse[Map[String, Int]]("{}") should equal(Map.empty[String, Int])
  }

  @Test def `A mutable.Map[String, Any] is not parsable from an empty JSON object in a JSON array` {
    evaluating {
      parse[Map[String, Any]]("[{}]")
    } should produce[ParsingException]
  }

  @Test def `A mutable.HashMap[String, Int] generates a JSON object` {
    generate(HashMap("one" -> 1)) should equal("""{"one":1}""")
  }

  @Test def `A mutable.HashMap[String, Int] is parsable from a JSON object with int field values` {
    parse[HashMap[String, Int]]("""{"one":1}""") should equal(HashMap("one" -> 1))
  }

  @Test def `A mutable.HashMap[String, Int] is parsable from an empty JSON object` {
    parse[HashMap[String, Int]]("{}") should equal(HashMap.empty[String, Int])
  }

  @Test def `A mutable.LinkedHashMap[String, Int] generates a JSON object` {
    generate(LinkedHashMap("one" -> 1)) should equal("""{"one":1}""")
  }

  @Test def `A mutable.LinkedHashMap[String, Int] is parsable from a JSON object with int field values` {
    parse[LinkedHashMap[String, Int]]("""{"one":1}""") should equal(LinkedHashMap("one" -> 1))
  }

  @Test def `A mutable.LinkedHashMap[String, Int] is parsable from an empty JSON object` {
    parse[LinkedHashMap[String, Int]]("{}") should equal(LinkedHashMap.empty[String, Int])
  }
}
