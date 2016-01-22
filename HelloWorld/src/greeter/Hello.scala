package greeter
import scala.annotation.tailrec

object Hello {

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)            //> fact: (n: Int)Int

  fact(15)                                        //> res1: Int = 2004310016

  def factorial(n: Int) = {
    @tailrec
    def fact2(n: Int, acc: Int): Int =
      if (n == 0) acc else fact2(n - 1, n * acc)
    
    fact2(n, 1)
  }
    
  def main(args: Array[String]): Unit = {
    println("Hello")
  }

}