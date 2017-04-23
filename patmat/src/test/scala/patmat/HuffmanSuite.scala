package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._
import java.io._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('a', 2)
    val char_list = string2Chars("hhhaaha0034l")
    val expected_freqs = List(('h', 4), ('a', 3), ('0', 2), ('3', 1), ('4', 1), ('l', 1))
    val expected_tree = Fork(Fork(Leaf('h', 4), Leaf('a', 3), List('h', 'a'), 7), Fork(Fork(Fork(Leaf('l', 1), Leaf('4', 1), List('l', '4'), 2), Leaf('3', 1), List('l', '4', '3'), 3), Leaf('0', 2), List('l', '4', '3', '0'), 5), List('h', 'a', 'l', '4', '3', '0'), 12)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("counting chars in input") {
    new TestTrees {
      assert(expected_freqs.forall(times(char_list).contains))
      assert(times(char_list).forall(expected_freqs.contains))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("check singleton list") {
    new TestTrees {
      assert(singleton(List(t3)))
      assert(!singleton(List()))
      assert(!singleton(List(t3, t3)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("test decoding") {
    new TestTrees {
      val testList: List[Bit] = List(0, 0, 0, 1, 1, 0, 1)
      assert(decode(expected_tree, testList) === List[Char]('h', 'a', '3'))
    }
  }

  test("test encoding") {
    new TestTrees {
      val testText: List[Char] = List('h', 'a', '3')
      assert(encode(expected_tree)(testText) === List[Bit](0, 0, 0, 1, 1, 0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("test conversion to table") {
    new TestTrees {
      val result = convert(expected_tree)
      val expected = List(('h', List(0, 0)), ('a', List(0, 1)), ('0', List(1, 1)), ('3', List(1, 0, 1)), ('l', List(1,0,0,0)), ('4', List(1,0,0,1)))
      expected.foreach(tuple => assert(result.contains(tuple)))
    }
  }

  test("combine of singleton or nil") {
    assert(combine(List(Leaf('a', 1))) == List(Leaf('a', 1)))
    assert(combine(Nil) == Nil)
  }

  test("quick encode simple") {
    new TestTrees {
      val testText: List[Char] = List('h', 'a', '3')
      assert(quickEncode(expected_tree)(testText) === List[Bit](0, 0, 0, 1, 1, 0, 1))
    }
  }

  test("quick encode == slow encode, french") {
    val testList = List('a', 'b', 'c')
    assert(quickEncode(frenchCode)(testList) === encode(frenchCode)(testList))
  }

}
