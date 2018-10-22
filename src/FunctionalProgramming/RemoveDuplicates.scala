package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */

import scala.collection.immutable.HashSet
object RemoveDuplicates {

  def filter(s : String) : String = {
    def helper(curString : List[Char], resString : List[Char], set : HashSet[Char]) : List[Char] = {
      curString match {
        case c :: cs => {
          if (set contains c) {
            helper(cs, resString, set)
          }
          else {
            helper(cs, c :: resString, set + c)
          }
        }
        case _ => {
          resString.reverse
        }
      }
    }
    helper(s.toList, Nil, HashSet[Char]()).mkString
  }

  def run : Unit = {
    println(filter(readLine()))
  }

  def main(args : Array[String]) : Unit = {
    run
  }
}
