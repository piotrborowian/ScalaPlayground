package week2

object sessionW2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int
  
  sum(x=>x)(1,4)                                  //> res0: Int = 10
  
  
  
  def sum2(f: Int => Int)(a: Int, b: Int) : Int = {
  	if(a > b) 0
  	else f(a) + sum(f)(a + 1, b)
  }                                               //> sum2: (f: Int => Int)(a: Int, b: Int)Int
  
  def product(f : Int => Int)(a: Int, b: Int) : Int = {
  	val productF = product(f) _
  	if(a > b) 1
  	else f(a) * productF(a +1 , b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
 
  def factorial(n: Int) : Int = product(x=>x)(1, n)
                                                  //> factorial: (n: Int)Int
  factorial(5)                                    //> res1: Int = 120
  
  def general(f : Int => Int) ( op : Int => Int => Int) (a : Int, b: Int) : Int = {
  	if(a > b) 0
  	else op(f(a))(general(f)(op)(a + 1,b))
  }                                               //> general: (f: Int => Int)(op: Int => (Int => Int))(a: Int, b: Int)Int
  
  case class Rational(num : Int, denom : Int){
  	def + (that : Rational) = Rational (num*that.denom + denom*that.num, denom * that.denom)
  	def neg = Rational (-num, denom)
  	def - (that : Rational) = this + (that.neg)
  	
  }
  
  println(Rational(1,3) - Rational(5,7) - Rational(3,2))
                                                  //> Rational(-79,42)
                                                  
  case class I(i : Int){
  	println(i)
  }
 
  case class E(a : I) {
  	def f(b : I) {
  		println("a = " + a + ", b = " + b);
  	}
  }
  
  E(I(42)).f(I(75))                               //> 42
                                                  //| 75
                                                  //| a = I(42), b = I(75)
}