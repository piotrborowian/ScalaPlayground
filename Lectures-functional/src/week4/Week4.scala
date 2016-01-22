package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException()
  def successor = new Succ(this)
  
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) that else throw new NoSuchElementException()
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = {
    def inc(acc: Nat, that : Nat) : Nat = {
      if (that.isZero) acc
      else inc(acc.successor, that.predecessor)
    }
    inc(this,that)
  }
  def - (that: Nat): Nat = {
    def dec(acc: Nat, that : Nat) : Nat = {
      if (that.isZero) acc
      else dec(acc.predecessor, that.predecessor)
    }
    dec(this,that)
  }
}

object Week4 {

  def main(args: Array[String]): Unit = {}

}