package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */

import scala.collection.immutable.HashMap
object CommonDivisors {

  def genPrimes(s : Stream[Int]) : Stream[Int] = {
    s.head #:: s.tail.filter(_ % s.head != 0)
  }

  def primeFactors(n : Int, primes : Stream[Int]) : HashMap[Int, Int] = {
    def helper(current : Int, curPrime : Stream[Int], map : HashMap[Int, Int]) : HashMap[Int, Int] = {
      if (current == 1) {
        map
      }
      else {
        if (scala.math.sqrt(current).toInt < curPrime.head) {
          val curVal = map.getOrElse(current, 0)
          map + (current -> (curVal + 1))
        }
        else {
          if (current % curPrime.head == 0) {
            val curVal = map.getOrElse(curPrime.head, 0)
            helper(current / curPrime.head, curPrime, map + (curPrime.head -> (curVal + 1)))
          }
          else {
            helper(current, curPrime.tail, map)
          }
        }
      }
    }
    helper(n, primes, new HashMap[Int, Int]())
  }

  def findGcdFactors(a : HashMap[Int, Int], b : HashMap[Int, Int]) : HashMap[Int, Int] = {
    def helper(keys : Set[Int], map : HashMap[Int, Int]) : HashMap[Int, Int] = {
      if (keys isEmpty) {
        map
      }
      else {
        (a.get(keys.head), b.get(keys.head)) match {
          case (Some(aVal), Some(bVal)) => {
            helper(keys.tail, map + (keys.head -> scala.math.min(aVal, bVal)))
          }
          case (Some(aVal), None) => {
            helper(keys.tail, map + (keys.head -> aVal))
          }
          case (None, Some(bVal)) => {
            helper(keys.tail, map + (keys.head -> bVal))
          }
        }
      }
    }
    val aKeys = a.keySet
    val bKeys = b.keySet
    val allKeys = aKeys & bKeys
    helper(allKeys, new HashMap[Int, Int]())
  }

  def findNumCombinations(map : HashMap[Int, Int]) : Int = {
    def helper(values : List[Int], retVal : Int) : Int = {
      values match {
        case x :: xs => {
          helper(xs, retVal * (x + 1))
        }
        case _ => {
          retVal
        }
      }
    }
    helper(map.values.toList, 1)
  }



  def run : Unit = {
    val primes = genPrimes(Stream.from(2))
    val t = readInt()
    for (i <- 0 until t) {
      val line = readLine().split(" ").map(_ toInt)
      val aFactors = primeFactors(line(0), primes)
      val bFactors = primeFactors(line(1), primes)
      val gcd = findGcdFactors(aFactors, bFactors)
      val combinations = findNumCombinations(gcd)

      println(combinations)
    }

  }

  def main(args : Array[String]) : Unit = {
    run

  }

}
