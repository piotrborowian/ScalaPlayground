package week4

trait PList[+T] {
  def isEmpty : Boolean
  def head : T
  def tail : PList[T]
  def :: [E >: T] (e : E) : PList[E]
  
  def last : T = this match {
  	case PNil => throw new NoSuchElementException
  	case x::PNil => x
  	case x::xs => xs.last
  }
  
  def :::[E >: T] (xs : PList[E]) : PList[E] = xs match {
  	case PNil => this
  	case y::ys => y:: ys:::this
  }
  
}

case object PNil extends PList[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
  def :: [E >: Nothing] (e : E) : PList[E] = week4.::(e, this)
}

case class ::[T](val head: T, val tail : PList[T]) extends PList[T] {
  def isEmpty = false  
  def :: [E >: T] (e : E) =  week4.::(e, this)
}
