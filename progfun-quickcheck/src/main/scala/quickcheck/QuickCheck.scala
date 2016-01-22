package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
    
  property("insert min and max") = forAll { a : Int =>
  	forAll { b : Int =>
  	  val minE = Math.min(a, b)
  	  val maxE = Math.max(a, b)
  	  val h = insert(minE, insert(maxE, empty))
  	  findMin(h) == minE && findMin(deleteMin(h)) == maxE && isEmpty(deleteMin(deleteMin(h)))
  	}
  }
  
  def getAll(h : H) : List[A] = {
    if(isEmpty(h)) Nil
    else {
      val head = findMin(h)
  	  head::getAll(deleteMin(h))
    }
  }
  
  property("take all elements and check if sorted") = forAll { h : H  =>
  	val sortedElements = getAll(h)
  	sortedElements.sorted == sortedElements
  }
  
  property("meld two lists and check min") = forAll { h1 : H  =>
  	forAll { h2 : H =>
  	  val min1 = findMin(h1)
  	  val min2 = findMin(h2)
  	  val minG = Math.min(min1, min2)
  	  val maxG = Math.max(min1, min2)
  	  val h12 = meld(h1, h2)
  	  findMin(h12) == minG
  	}
  }
  
  property("meld two lists and check all elems") = forAll { h1 : H  =>
  	forAll { h2 : H =>
  	  val l1  = getAll(h1)
  	  val l2 = getAll(h2)
  	  val h12 = meld(h1, h2)
  	  val l12 = getAll(h12)
  	  (l12.toSet) == (l1 ++ l2).toSet
  	}
  }
  
  lazy val genHeap: Gen[H] = for {
    head <- arbitrary[Int]
    tail <- oneOf(value(empty), genHeap)
  } yield insert(head, tail)
 

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
}
