package objsets

import TweetReader._

/**
 * A class to represent ets.
 */
class Tweet1(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet1{
  
  def isEmpty: Boolean
  
  def filter(p: Tweet1 => Boolean): TweetSet1 = 
      filterAcc(p,new Empty1)
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet1 => Boolean, acc: TweetSet1): TweetSet1
  
  def union(that: TweetSet1): TweetSet1
  /**
  def most: Int
  */  
  def mostRetweeted: Tweet1
  
  def descendingByRetweet: TweetList1

  def incl(tweet: Tweet1): TweetSet1

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet1): TweetSet1

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet1): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet1 => Unit): Unit
}

class Empty1 extends TweetSet1 {
  
  def isEmpty = true
  def filterAcc(p: Tweet1 => Boolean, acc: TweetSet1) = acc
  def union(that: TweetSet1) = that
  def mostRetweeted = throw new java.util.NoSuchElementException
  def descendingByRetweet: TweetList1 = Nil1
  def incl(tweet: Tweet1) = new NonEmpty1(tweet,new Empty1, new Empty1)
  def remove(tweet: Tweet1) = this
  def contains(tweet: Tweet1) = false
  def foreach(f: Tweet1 => Unit): Unit = ()
  
}

class NonEmpty1 (elem: Tweet1, left:TweetSet1, right: TweetSet1) extends TweetSet1 {
  def isEmpty = false
  def filterAcc(p: Tweet1 => Boolean, acc:TweetSet1): TweetSet1 = { 
    if (p(elem)) (left filterAcc(p, acc incl elem)) union (right filterAcc(p, acc incl elem))
    else (left filterAcc(p, acc)) union (right filterAcc(p,acc))
  }
  def union(that: TweetSet1): TweetSet1 = (left union (right union that)) incl elem 
  def incl(tweet: Tweet1): TweetSet1 = {
    if (tweet.text < elem.text) new NonEmpty1(elem, left incl tweet, right)
    else if (tweet.text > elem.text) new NonEmpty1(elem, left, right incl tweet)
    else this
  }
  def remove(tweet: Tweet1): TweetSet1 = {
    if (tweet.text < elem.text) new NonEmpty1(elem, left remove tweet, right)
    else if (tweet.text > elem.text) new NonEmpty1(elem, left, right remove tweet)
    else left union right
  }
  def foreach(f: Tweet1 => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  def contains(tweet: Tweet1): Boolean = {
    if (tweet.text < elem.text) left contains tweet
    else if (tweet.text > elem.text) right contains tweet
    else true
  }
  
  def mostRetweeted: Tweet1 = {
    val l = if(left.isEmpty) elem else left.mostRetweeted
    val r = if(right.isEmpty) elem else right.mostRetweeted
    val x = elem.retweets
    val y = l.retweets
    val z = r.retweets
    if(x>y) (if(x>z) elem else r)
    else if (y >z) l
    else r
  }
  
  def descendingByRetweet: TweetList1 = {
    new Cons1(mostRetweeted, (this remove mostRetweeted).descendingByRetweet)
  }
}

trait TweetList1 {
  def head: Tweet1
  def tail: TweetList1
  def isEmpty: Boolean
  def foreach(f: Tweet1 => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil1 extends TweetList1 {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons1(val head: Tweet1, val tail: TweetList1) extends TweetList1 {
  def isEmpty = false
}

object GoogleVsApple1 {
  val every1 = TweetReader.allTweets
  val google1 = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple1 = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  
  lazy val googleTweets: TweetSet = every1.filter(tweet => google1.exists(tweet.text.contains))
  lazy val appleTweets: TweetSet = every1.filter(tweet => apple1.exists(tweet.text.contains))
    
  lazy val trending: TweetSet1 = (googleTweets union appleTweets).descendingByRetweet
}





