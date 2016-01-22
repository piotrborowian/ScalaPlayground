package greeter

class Base1(x : Int, y: Int) 

class Sub1(a : Int) extends {
  //you can put arbitrary complex calculations of x here
  val x = a * 2
} with Base1(x, a)


object CompanionObjects {

  def main(args: Array[String]): Unit = {}

}
