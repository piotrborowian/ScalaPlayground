package week1

object ws {
  val x : Int => Int = Map(1 -> 7, 3 -> 24)       //> x  : Int => Int = Map(1 -> 7, 3 -> 24)
  
  val y : PartialFunction[Int,Int] =  Map(1 -> 7, 3 -> 24)
                                                  //> y  : PartialFunction[Int,Int] = Map(1 -> 7, 3 -> 24)
  
  x(1)                                            //> res0: Int = 7
  y.isDefinedAt(1)                                //> res1: Boolean = true
  y.isDefinedAt(4)                                //> res2: Boolean = false
  
}