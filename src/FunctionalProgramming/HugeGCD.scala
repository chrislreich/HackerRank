package FunctionalProgramming

/**
  * Created by creich on 4/3/18.
  */
import scala.collection.mutable.HashMap
object HugeGCD {

  val mod = 1000000007

  def genPrimes(s : Stream[Int]) : Stream[Int] = {
    s.head #:: s.tail.filter(_ % s.head != 0)
  }

  def primeFactorize(input : Int, primes : Stream[Int]) : List[(Int, Int)] = {
    def helper(current : Int, curPrimes : Stream[Int], retList : List[(Int, Int)]) : List[(Int, Int)] = {
      if (curPrimes.head > scala.math.sqrt(current).toInt) {
        ((current, 1) :: retList).reverse
      }
      else {
        if (current == 1) {
          retList.reverse
        }
        else {
          if (current % curPrimes.head == 0) {
            if (retList isEmpty) {
              helper(current / curPrimes.head, curPrimes, (curPrimes.head, 1) :: retList)
            }
            else {
              if (curPrimes.head == retList.head._1) {
                helper(current / curPrimes.head, curPrimes, (curPrimes.head, 1 + retList.head._2) :: retList.tail)
              }
              else {
                helper(current / curPrimes.head, curPrimes, (curPrimes.head, 1) :: retList)
              }
            }
          }
          else {
            helper(current, curPrimes.tail, retList)
          }
        }
      }
    }

    helper(input, primes, Nil)
  }

  def combinePrimeFactors(productPrimes : List[List[(Int, Int)]]) : HashMap[Int, Int] = {
    val retMap = new HashMap[Int, Int]()
    for (list <- productPrimes) {
      for (e <- list) {
        val res : Int = retMap.getOrElse(e._1, 0)
        retMap += (e._1 -> (res + e._2))
      }
    }
    retMap
  }

  def combineMaps(a : HashMap[Int, Int], b : HashMap[Int, Int]) : List[(Int, Int)] = {
    val aKeys = a.keySet
    val bKeys = b.keySet
    val intersect = aKeys & bKeys

    val retMap = new HashMap[Int, Int]()

    for (k <- intersect) {
      val aVal = a.get(k) match {case Some(v) => v}
      val bVal = b.get(k) match {case Some(v) => v}
      retMap += (k -> scala.math.min(aVal, bVal))
    }
    retMap.toList
  }

  def findVal(input : List[(Int, Int)]) : Int = {
    input.foldLeft(1L)((b, t) => {var curNum = t._1.toLong; for (i <- 0 until t._2 - 1) {curNum *= t._1; curNum = curNum % mod}; (curNum * b) % mod }).toInt
  }


  def run : Unit = {
    val n = readInt()
    val nLine = readLine.split(" ").map(_ toInt).toList
    val m = readInt()
    val mLine = readLine.split(" ").map(_ toInt).toList

    val primes = genPrimes(Stream.from(2))

    val nPrimes = for (nInt <- nLine) yield primeFactorize(nInt, primes)
    val mPrimes = for (mInt <- mLine) yield primeFactorize(mInt, primes)

    val nFactors = combinePrimeFactors(nPrimes)
    val mFactors = combinePrimeFactors(mPrimes)

    val gcdPrimes = combineMaps(nFactors, mFactors)

    val finalVal = findVal(gcdPrimes) % mod

    println(finalVal)
  }

  def main(args : Array[String]) : Unit = {
    run
  }
}
