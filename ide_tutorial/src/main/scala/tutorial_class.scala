/**
  * Created by Owen James on 3/28/2017.
  */
package example

object tutorial_class extends App {
  println("Hello world!")
}

object HelloWorld {
  def main(args: Array[String]) {
    println(sqrt(0.001))
    println(sqrt(0.1E-20))
    println(sqrt(1E20))
    println(sqrt(1E50))
    println(factorial(4))
  }

  def sqrt(x: Double): Double = {

    def abs(x: Double): Double =
      if (x >= 0) x
      else -x

    def isGoodEnough(guess: Double, x: Double): Boolean =
      abs(guess * guess - x) < 0.001*x

    def improve(guess: Double, x: Double): Double =
      (guess + x / guess) / 2

    def squareRootIteration(guess: Double, x: Double): Double =
      if(isGoodEnough(guess, x)) guess
      else squareRootIteration(improve(guess, x), x)

    squareRootIteration(1.0, x)

  }

  def factorial(n: Int): Int = {

    def factorialIteration(n: Int, accumulator: Int): Int = {
      if (n == 0) accumulator else factorialIteration(n - 1, accumulator * n)
    }

    factorialIteration(n, 1)

  }

}