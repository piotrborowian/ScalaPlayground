package recfun
import common._

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
    require (r < 0 || c < 0 || c > r)
      
    def pascalRec(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1
	  else pascal(c-1, r-1) + pascal(c, r-1)
	  
	pascalRec(c, r)  
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(openingParans: Int, chars: List[Char]): Boolean = {
      if (openingParans < 0) false
      else if (chars.isEmpty) openingParans == 0
      else {
        if (chars.head == ')')
          balanceRec(openingParans - 1, chars.tail)
        else if (chars.head == '(')
          balanceRec(openingParans + 1, chars.tail)
        else
          balanceRec(openingParans, chars.tail)
      }
    }

    balanceRec(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + 
    	 countChange(money, coins.tail)
  }
  
}
