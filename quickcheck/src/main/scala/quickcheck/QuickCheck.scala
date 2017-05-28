package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("ins1") = forAll { (a: A, b: A) =>
    findMin(insert(b, insert(a, empty))) == ord.min(a, b)
  }

  property("del1") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("del2") = forAll { (a: A, b: A) =>
    ord.max(a, b) == findMin(deleteMin(insert(b, insert(a, empty))))
  }

  property("del3") = forAll { (a: A, b: A, c: A) =>
    val sorted = List(a, b, c).sorted(ord)
    val heap = insert(a, insert(b, insert(c, empty)))
    findMin(heap) == sorted.head && findMin(deleteMin(heap)) == sorted.tail.head
  }

  property("meld1") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == findMin(a) || findMin(meld(a, b)) == findMin(b)
  }

  property("rec1") = forAll { (a: H) =>
    def goDeeper(h: H, elems: List[A]): List[A] = {
      if (isEmpty(h)) elems
      else goDeeper(deleteMin(h), findMin(h) :: elems)
    }
    val elems = goDeeper(a, List()).reverse
    elems == elems.sorted(ord)
  }

}
