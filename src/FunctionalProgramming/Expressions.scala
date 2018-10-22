package FunctionalProgramming

/**
  * Created by creich on 2/27/18.
  */

import scala.collection.immutable.HashMap
object Expressions {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def process(list : List[Int]) : String = {

  def newStates(old : HashMap[Int, List[Char]], n : Int) : HashMap[Int, List[Char]] = {
    val nAsString = n.toString.reverse.toList
    def mapMultiply(t : (Int, List[Char])) : (Int, List[Char]) = {
      ((t._1 * n) % 101, nAsString ::: ('*' :: t._2))
    }

    def mapAdd(t : (Int, List[Char])) : (Int, List[Char]) = {
      ((t._1 + n) % 101, nAsString ::: ('+' :: t._2))
    }

    def mapSubtract(t : (Int, List[Char])) : (Int, List[Char]) = {
      ((t._1 - n) % 101, nAsString ::: ('-' :: t._2))
    }


    if (old.size == 101) {
      old.map(mapAdd)
    }
    else {
      val plusMap = old.map(mapAdd)
      val timesMap = old.map(mapMultiply)
      val minusMap = old.map(mapSubtract)
      (plusMap.merged(timesMap)(null)).merged(minusMap)(null)
    }

  }


    def go(currentList : List[Int], currentStates : HashMap[Int, List[Char]]) : HashMap[Int, List[Char]] = {
      currentList match {
        case x :: xs => {
          val nextStates = newStates(currentStates, x)
          go(xs, nextStates)
        }
        case _ => {
          currentStates
        }
      }
    }

    val map = new HashMap[Int, List[Char]]() + (list.head -> list.head.toString.reverse.toList)

    val result = go(list.tail,map)

    val winner = result.getOrElse(0, Nil)
    winner.reverse.mkString
  }


  def run1 : Unit = {
    val t = readInt()
    val inputList = readLine().split(" ").toList.map(_.toInt)

    val result = process(inputList)
    println(result)
  }

  def main(args : Array[String]) : Unit = {
    run1

  }

}
