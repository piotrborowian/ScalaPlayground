object WorkSheet {
  3.toBinaryString                                //> res0: String = 11
  16.toBinaryString                               //> res1: String = 10000
  
  def intToBinaryList(i : Int, listSize : Int) : List[Boolean] = {
    val trailing = (i.toBinaryString.map { _ == '1'}).toList
    val leading = List.fill(listSize - trailing.length)(false)
    leading ++ trailing
  }                                               //> intToBinaryList: (i: Int, listSize: Int)List[Boolean]
  
  intToBinaryList(3, 4)                           //> res2: List[Boolean] = List(false, false, true, true)
  
  List(1,2,40).flatMap {
    case 1 => List(77)
  }                                               //> scala.MatchError: 2 (of class java.lang.Integer)
                                                  //| 	at WorkSheet$$anonfun$main$1$$anonfun$3.apply(WorkSheet.scala:13)
                                                  //| 	at WorkSheet$$anonfun$main$1$$anonfun$3.apply(WorkSheet.scala:13)
                                                  //| 	at scala.collection.TraversableLike$$anonfun$flatMap$1.apply(Traversable
                                                  //| Like.scala:251)
                                                  //| 	at scala.collection.TraversableLike$$anonfun$flatMap$1.apply(Traversable
                                                  //| Like.scala:251)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at scala.collection.TraversableLike$class.flatMap(TraversableLike.scala:
                                                  //| 251)
                                                  //| 	at scala.collection.AbstractTraversable.flatMap(Traversable.scala:105)
                                                  //| 	at WorkSheet$$anonfun$main$1.apply$mcV$sp(WorkSheet.scala:13)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$e
                                                  //| Output exceeds cutoff limit.
}