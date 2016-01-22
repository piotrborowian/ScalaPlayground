package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
    
  def max(that : Tweet) : Tweet = if (retweets > that.retweets) this else that
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   //def union(that: TweetSet): TweetSet = filterAcc(_ => true, that)
   
   def union(that: TweetSet): TweetSet = addTo(that)
   def addTo(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = {
    val mostCandidate = mostRetweetedNoExcp
    if (mostCandidate eq NoSuchElementTweet) throw new NoSuchElementException
    else mostCandidate
  }
  
  def mostRetweetedNoExcp : Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {
    try {
      val maxTweet = mostRetweeted
      val tail = remove(maxTweet).descendingByRetweet
      new Cons(maxTweet, tail)
    } catch {
      case e : NoSuchElementException => Nil
    }
  }


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
  
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, Empty, Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  def mostRetweetedNoExcp: Tweet = NoSuchElementTweet
 
  def addTo(that : TweetSet) : TweetSet = that
}

object Empty extends Empty

object NoSuchElementTweet extends Tweet("","",-1)

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val children =  right.filterAcc(p, left.filterAcc(p, acc)) 
    if (p(elem)) children.incl(elem)
    else children
  }
      
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  
  def mostRetweetedNoExcp : Tweet = left.mostRetweetedNoExcp.max(right.mostRetweetedNoExcp).max(elem)
  
  def addTo(that : TweetSet) : TweetSet = {
	/* The semantics of this function is that we get an accumulator (that) here and we want to insert 
	 * ourselves to this accumulator one by one, first the current element and then the children
	 * 
	 * so first you add yourself:
	 * 	that.incl(elem)
	 *  
	 * then you pass the new accumulator to the children, so that they can add themselves too:
	 * 	right.union(that.incl(elem)) 
	 * 
	 * and you get an accumulator that already includes right children. Now the left children have to
	 * insert themselves too:
	 * 	left.union(right.union(that.incl(elem)) 
	 * 
	 */
    val thatWithElem = that.incl(elem)							 //O(1)
    val thatWithElemAndLeftChildren = left.addTo(thatWithElem)   //the number of left children
    val thatWithElemAndLeftRightChilderen = right.addTo(thatWithElemAndLeftChildren) //the number of right children
    //thatWithElemAndLeftRightChilderen
    
    /*
     * So you want to add  yourself one by one to that. How about this ? 
     * left.union(right).union(that).incl(elem)
     * 
     * here it is reversed, you insert yourself, but then in every recursive call the left children add themselves
     * to the right children, but this is just a temporary object that is then thrown away, 
     */ 
    val leftWithRight = left.addTo(right) //here you always add all the left children to right children before they add themselves = 0(n)
    val leftWithRightWithThat = leftWithRight.addTo(that) // here you finally add them 
    val leftWithRightWithThatWithElem = leftWithRightWithThat.incl(elem) //and include the element 1
    leftWithRightWithThatWithElem
  }

}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  val allTweets = TweetReader.allTweets
  
  def isKeywordMentioned(keywords: List[String])(tweet: Tweet) : Boolean = { 
    keywords.find(tweet.text.contains(_)).isDefined
  }
  
  lazy val googleTweets: TweetSet = allTweets.filter(isKeywordMentioned(google))
  lazy val appleTweets: TweetSet = allTweets.filter(isKeywordMentioned(apple))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
