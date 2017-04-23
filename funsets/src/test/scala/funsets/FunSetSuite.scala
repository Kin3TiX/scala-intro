package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements in either set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements in both sets") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val i = intersect(u1, u2)
      assert(!contains(i, 1), "Intersect 1")
      assert(contains(i, 2), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
    }
  }

  test("difference contains elements of a not in b") {
    new TestSets {
      val a = union(union(s1, s2), s3)
      val b = s3
      val d = diff(a, b)
      assert(contains(d, 1), "Difference 1")
      assert(contains(d, 2), "Difference 2")
      assert(!contains(d, 3), "Difference 3")
    }
  }

  test("filter is the subset elements in a satisfied by p") {
    new TestSets {
      val a = union(union(s1, s2), s3)
      def p = (element: Int) => element != 2
      val f = filter(a, p)
      assert(contains(f, 1), "Filtered 1")
      assert(!contains(f, 2), "Filtered 2")
      assert(contains(f, 3), "Filtered 3")
    }
  }

  test("forall tests that p is true for all a") {
    new TestSets {
      val a = union(union(s1, s2), s3)
      def p = (element: Int) => element > 0 && element < 5
      def np = (element: Int) => element > 10
      val f1 = forall(a, p)
      val f2 = forall(a, np)
      assert(f1, "Forall 1")
      assert(!f2, "Forall 2")
    }
  }

  test("exists tests that p is true for any a") {
    new TestSets {
      val a = union(union(s1, s2), s3)
      def p = (element: Int) => element == 2
      def np = (element: Int) => element < 0
      val e1 = exists(a, p)
      val e2 = exists(a, np)
      assert(e1, "Exists 1")
      assert(!e2, "Exists 2")
    }
  }

  test("map transforms all a by p") {
    new TestSets {
      val a = union(union(s1, s2), s3)

      def p1 = (element: Int) => element*2
      val m1 = map(a, p1)
      def c1 = (element: Int) =>
        element == 2 || element == 4 || element == 6
      assert(forall(m1, c1), "Map 1")

      def p2 = (element: Int) => element-3
      val m2 = map(a, p2)
      def c2 = (element: Int) =>
        element == -2 || element == -1 || element == 0
      assert(forall(m2, c2), "Map 2")

    }
  }

}
