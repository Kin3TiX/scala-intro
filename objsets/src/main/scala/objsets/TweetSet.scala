package objsets

import TweetReader._
import objsets.GoogleVsApple.appleTweets

/*
Class representing a tweet
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/*
Class representing a binary tree of tweets
 */
abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, this)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def foreach(f: Tweet => Unit): Unit

}


/*
Class representing an empty tweet set
 */
class Empty extends TweetSet {

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  override def union(that: TweetSet): TweetSet = that

  override def mostRetweeted: Tweet = throw new NoSuchElementException

  override def descendingByRetweet: TweetList = Nil

  override def contains(tweet: Tweet): Boolean = false

  override def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  override def remove(tweet: Tweet): TweetSet = this

  override def foreach(f: Tweet => Unit): Unit = ()

}


/*
Class representing a non empty tweet set
 */
class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if(!p(elem)) left.filterAcc(p, right.filterAcc(p, acc.remove(elem)))
    else left.filterAcc(p, right.filterAcc(p, acc))
  }

  override def union(that: TweetSet): TweetSet = {
    left.union(right.union(that.incl(elem)))
  }

  override def mostRetweeted: Tweet = {
    val leftMost = try {left.mostRetweeted} catch { case _: NoSuchElementException => elem}
    val rightMost = try {right.mostRetweeted} catch { case _: NoSuchElementException => elem}
    if(elem.retweets > leftMost.retweets && elem.retweets > rightMost.retweets) elem
    else if(leftMost.retweets > rightMost.retweets) leftMost
    else rightMost
  }

  override def descendingByRetweet: TweetList = {
    new Cons(mostRetweeted, this.remove(mostRetweeted).descendingByRetweet)
  }

  override def contains(x: Tweet): Boolean = {
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true
  }

  override def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  override def remove(tw: Tweet): TweetSet = {
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  }

  override def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

}


/*
Trait representing a tweet list
 */
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


/*
Object representing the nil of a tweet list
 */
object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}


/*
Class representing the cons of a tweet list
 */
class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


/*
Functions to analyze tweets related to google and apple
 */
object GoogleVsApple {

  // Define keywords relevant to apple and google mentions
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  // Filter all tweets by those where the text contains google keywords
  lazy val googleTweets: TweetSet =
    TweetReader.allTweets.filter(
      tweet => google.exists(
        keyword => tweet.text.contains(keyword)))

  // Filter all tweets by those where the text contains apple keywords
  lazy val appleTweets: TweetSet =
    TweetReader.allTweets.filter(
      tweet => apple.exists(
        keyword => tweet.text.contains(keyword)))

  // List tweets ordered by retweets from the union of google and apple tweetsets
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet

}


/*
Application to analyze provided tweetsets
 */
object Main extends App {
  GoogleVsApple.trending foreach println
}
