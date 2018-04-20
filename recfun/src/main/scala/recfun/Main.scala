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
    def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case (c, r) if (c == r) => 1
      case (c, r) => pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceHelper(parens: List[Char], chars: List[Char]): Boolean = {
        chars match {
          case '(' :: tail => balanceHelper('(' :: parens, tail)
          case ')' :: _ if parens.isEmpty => false
          case ')' :: _ if parens.head != '(' => false
          case ')' :: tail => balanceHelper(parens.tail, tail)
          case Nil => parens.isEmpty
          case _ :: tail => balanceHelper(parens, tail)
        }
      }

      balanceHelper(List(), chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else if (coins.min > money) 0
      else coins.map(i => if (i <= money) countChange(money - i, coins.filter(_ >= i)) else 0).sum
    }
  }
