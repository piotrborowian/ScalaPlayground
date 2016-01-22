package week6

object session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val pouring = Pouring(Vector(4, 9, 173), 21)    //> pouring  : week6.Pouring = Pouring(Vector(4, 9, 173),21)
  
  pouring.solutions                               //> res0: Stream[week6.session.pouring.Path] = Stream(Fill(0) Fill(1) Pour(0,2) 
                                                  //| Fill(0) Pour(0,2) Fill(0) Pour(0,2) Pour(1,2)-->Vector(0, 0, 21), size = 8
                                                  //| , ?)
}