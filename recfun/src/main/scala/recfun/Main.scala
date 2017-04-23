package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(r == 0 || c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIterator(chars: List[Char], balancer: Int): Boolean = {
        if(balancer < 0) false
        else if(chars.isEmpty) balancer == 0
        else if(chars.head == '(') balanceIterator(chars.tail, balancer+1)
        else if(chars.head == ')') balanceIterator(chars.tail, balancer-1)
        else balanceIterator(chars.tail, balancer)
      }
      balanceIterator(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty || money < 0) 0
      else if(money == 0) 1
      else countChange(money, coins.tail) + countChange(money-coins.head, coins)
    }
  }
