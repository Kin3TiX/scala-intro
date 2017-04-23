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

  test("tree generation") {
    new TestTrees {
      generateGraph(expected_tree, "expected.dot")
      val generated = createCodeTree(char_list)
      generateGraph(generated, "generated.dot")
      assert(generated === expected_tree)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  def generateGraph(tree: CodeTree, fileName: String) = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("digraph dtree {")
    printNodes(tree, bw)
    bw.write("\n")
    printEdges(tree, bw)
    bw.write("\n}")
    bw.close()
  }

  def printNodes(tree: CodeTree, bw: BufferedWriter): Unit = tree match {
    case Leaf(letter, weight) => {
      val inner = raw""""$letter\n$weight""""
      bw.write(s"\n\t$letter[label=$inner]")
    }
    case Fork(leftTree, rightTree, letter_list, weight) => {
      val letters = letter_list.mkString
      val inner = raw""""$letters\n$weight""""
      bw.write(s"\n\t$letters[label=$inner]")
      printNodes(leftTree, bw)
      printNodes(rightTree, bw)
    }
  }

  def printEdges(tree: CodeTree, bw: BufferedWriter): Unit = tree match {
    case Fork(leftTree, rightTree, letter_list, _) => {
      val src = letter_list.mkString
      leftTree match {
        case Fork(_, _, letter_list, _) => {
          val dest = letter_list.mkString
          bw.write(s"\n\t$src -> $dest;")
          printEdges(leftTree, bw)
        }
        case Leaf(dest, _) => bw.write(s"\n\t$src -> $dest;")
      }
      rightTree match {
        case Fork(_, _, letter_list, _) => {
          val dest = letter_list.mkString
          bw.write(s"\n\t$src -> $dest;")
          printEdges(rightTree, bw)
        }
        case Leaf(dest, _) => bw.write(s"\n\t$src -> $dest;")
      }
    }
  }

}
