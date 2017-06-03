package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  trait unsolvablePolynomial {
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(3.0)
  }

  trait singleRootPolynomial {
    val a = Var(2.0)
    val b = Var(4.0)
    val c = Var(2.0)
  }

  trait dualRootPolynomial {
    val a = Var(1.0)
    val b = Var(3.0)
    val c = Var(-4.0)
  }

  test("intermediary discriminant") {
    new unsolvablePolynomial {
      assert(Polynomial.computeDelta(a, b, c)() == -8.0)
    }
  }

  test("unsolvable polynomial") {
    new unsolvablePolynomial {
      assert(Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))() == Set())
    }
  }

  test("single root polynomial") {
    new singleRootPolynomial {
      assert(Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))() == Set(-1.0))
    }
  }

  test("dual root polynomial") {
    new dualRootPolynomial {
      assert(Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))() == Set(1, -4))
    }
  }

}
