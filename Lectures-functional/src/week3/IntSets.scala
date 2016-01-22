package week3

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "." 
}

case class NonEmpty(elem : Int, left : IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  
  def incl(x: Int): IntSet = {
    if (x < elem) NonEmpty(elem, left incl x, right)
    else if (x > elem) NonEmpty(elem, left, right incl x)
    else this 
  }
  
  def union(other: IntSet): IntSet = {    
    other union left union right incl elem
  }
  
  override def toString = "{" + left + elem + right + "}"
}

trait List[+T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
  def add[E >: T](e : E) : List[E]
  def nth(n : Int) : T
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
  def nth(n : Int) = throw new NoSuchElementException
  def add[E >: Nothing] (e : E) : List[E] = new Cons(e,this)
}

class Cons[+T](val head : T, val tail : List[T]) extends List[T] {
  def isEmpty = false
  def add[E >: T](e : E): List[E] = {
    def addH(e : E, xs : List[E]) : List[E] = new Cons(head, addH(e, tail)) 
    addH(e, this)
  }
  def nth(n: Int) : T = {
    if(isEmpty || n < 0) throw new IndexOutOfBoundsException
    if(n == 0) head
    else tail.nth(n - 1)
  }
}


object IntSets {

  def main(args: Array[String]): Unit = {
    val set = Empty incl 3 incl 7 incl 14
    val s1 = List()
    val set2 = Empty incl 2 incl 7 incl 24
    val unionset = set union set2
    
    //lists
    val l = (Nil).add(1).add(2).add(3)
    
    println(l)
  }

}