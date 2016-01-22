package week1

object session {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)           //> sqrtIter: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double) =
    abs(x / guess - guess) < 0.001                //> isGoodEnough: (guess: Double, x: Double)Boolean

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2                       //> improve: (guess: Double, x: Double)Double

  def sqrt(x: Double) = sqrtIter(1.0, x)          //> sqrt: (x: Double)Double

  sqrt(1e-20)                                     //> res0: Double = 9.765625000000035E-4

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n - 1)            //> fact: (n: Int)Int

  fact(15)                                        //> res1: Int = 2004310016

  def factorial(n: Int) = {
    def fact(n: Int, acc: Int): Int =
      if (n == 0) acc else fact(n - 1, n * acc)
    
    fact(n, 1)
  }                                               //> factorial: (n: Int)Int

  sqrt(1e50)                                      //> res2: Double = 1.0E25
  
}