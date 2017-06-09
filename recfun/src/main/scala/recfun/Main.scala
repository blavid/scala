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
  def pascal(c: Int, r: Int): Int =
    if (c > r) 0
    else if (c == 0) 1
    else return pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val rightParen = '('
    val leftParen = ')'

    def isBalanced(remainingChars: List[Char], leftParens: Int): Boolean = {
      if (remainingChars.isEmpty) leftParens == 0
      else if (remainingChars.head == leftParen) isBalanced(remainingChars.tail, leftParens + 1)
      else if (remainingChars.head == rightParen) leftParens > 0 && isBalanced(remainingChars.tail, leftParens - 1)
      else isBalanced(remainingChars.tail, leftParens)
    }
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (coins.isEmpty || money == 0) 0
      else 1
  }
