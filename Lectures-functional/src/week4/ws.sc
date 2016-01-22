package week4

object ws {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val l = 1::2::3::PNil                           //> Hallo PNil
                                                  //| Hallo ::
                                                  //| Hallo ::
                                                  //| l  : week4.PList[Int] = ::(1,::(2,::(3,PNil)))
  
  l match {
    case PNil => "empty"
    case x::xs => "non empty"
  }                                               //> res0: String = non empty
  
  l ::: (4::5::6::PNil)                           //> Hallo PNil
                                                  //| Hallo ::
                                                  //| Hallo ::
                                                  //| Hallo ::
                                                  //| Hallo ::
                                                  //| Hallo ::
                                                  //| res1: week4.PList[Int] = ::(1,::(2,::(3,::(4,::(5,::(6,PNil))))))
}