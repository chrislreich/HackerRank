package CrackingCoding

/**
  * Created by creich on 5/18/18.
  */

import scala.collection.immutable.HashMap
object CC_StringAnagrams {

  def mapStrings(map : HashMap[Char, Int], str : String) : HashMap[Char, Int] = {
    if (str isEmpty) {
      map
    }
    else {
      val previousNumber = map.getOrElse(str.head, 0)
      mapStrings(map + (str.head -> (previousNumber + 1)), str.tail)
    }
  }

  def compareMaps(m1 : HashMap[Char, Int], m2 : HashMap[Char, Int], sum : Int) : Int = {
    (m1.isEmpty, m2.isEmpty) match {
      case (true, true) => {
        sum
      }
      case (false, true) => {
        compareMaps(m1.tail, m2, sum + m1.head._2)
      }
      case (true, false) => {
        compareMaps(m1, m2.tail, sum + m2.head._2)
      }
      case (false, false) => {
        val head = m1.head
        val m2_val = m2.getOrElse(head._1, 0)
        val diff = scala.math.abs(head._2 - m2_val)
        compareMaps(m1.tail, m2 - m1.head._1, sum + diff)
        }
      }
    }


  def run = {
    val m1 = mapStrings(new HashMap[Char, Int](), readLine())
    val m2 = mapStrings(new HashMap[Char, Int](), readLine())

    val sum = compareMaps(m1, m2, 0)
    println(sum)


  }

  def main(args : Array[String]) : Unit = {
    run

  }

}
