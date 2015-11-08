package recfun

import scala.math.Ordered.orderingToOrdered

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
    if (c == 0 && r == 0) {
      1
    } else if (c == 0 || r == c) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceF(depth: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        depth == 0
      } else {
        if (chars.head == ')') {
          if (depth == 0) {
            false
          } else {
            balanceF(depth - 1, chars.tail)
          }
        } else if (chars.head == '(') {
          balanceF(depth + 1, chars.tail)
        } else {
          balanceF(depth, chars.tail)
        }
      }
    }

    balanceF(0, chars)
  }



  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        1
      else if(money < 0)
        0
      else if(coins.isEmpty && money>=1 )
        0
      else
        count(money, coins.tail) + count(money - coins.head, coins)
    }

    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
