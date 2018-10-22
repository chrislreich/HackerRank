package Algorithms

/**
  * Created by creich on 6/5/18.
  */
object BetweenSets {

  def genPrimes(stream : Stream[Int]) : Stream[Int] = {
    stream.head #:: genPrimes(stream.tail.filter(x => (2 to x - 1).forall(x % _ != 0)))
  }


  def leastCommonMultiplePrimes(input : List[Map[Int, Int]]) : Map[Int, Int] = {
    def helper(curMap : Map[Int, Int], newMap : List[(Int, Int)]) : Map[Int, Int] = {
      newMap match {
        case x :: xs => {
          val previousValue = curMap.getOrElse(x._1, 0)
          if (x._2 > previousValue) {
            helper(curMap + (x._1 -> x._2), xs)
          }
          else {
            helper(curMap, xs)
          }
        }
        case _ => {
          curMap
        }
      }
    }
    input.map(_.toList).foldLeft(Map[Int, Int]())(helper)
  }

  def greatestCommonDivisorFactors(input : List[Map[Int, Int]]) : Map[Int, Int] = {
    def helper(map : Map[Int, Int], curList : Map[Int, Int]) : Map[Int, Int] = {
      val keySetIntersection = map.keySet & curList.keySet

      val newTuples = for (v <- keySetIntersection)
       yield (map.get(v), curList.get(v)) match {
          case (Some(l), Some(r)) => {
            if (l < r) {
              (v, l)
            }
            else (v, r)
          }
      }
      newTuples.toMap
    }

    input.tail.foldLeft(input.head)(helper)
  }


  def primeFactorize(input : List[Int], primes : Stream[Int]) : List[Map[Int, Int]] = {
    def primeFactors(num : Int, primes : Stream[Int], map : Map[Int, Int]) : Map[Int, Int] = {
      if (num == 1) {
        map
      }
      else {
        if (num % primes.head == 0) {
          val previousValue = map.getOrElse(primes.head, 0)
          primeFactors(num / primes.head, primes, map + (primes.head -> (previousValue + 1)))
        }
        else {
          primeFactors(num, primes.tail, map)
        }
      }
    }
    input.map(primeFactors(_, primes, Map[Int, Int]()))
  }

  def findNumbers(lcm : Map[Int, Int], gcd : Map[Int, Int]) : Int = {
    def helper(gcdList : List[(Int, Int)], retList : List[Int]) : Option[List[Int]] = {
      gcdList match {
        case x :: xs => {
          lcm.get(x._1) match {
            case Some(v) => {
              if (v > x._2) {
                None
              }
              else {
                helper(xs, ((x._2 - v) + 1) :: retList)
              }
            }
            case None => {
              helper(xs, (x._2 + 1) :: retList)
            }
          }
        }
        case _ => {
          Some(retList)
        }
      }
    }

    val gcdKeys = gcd.keySet
    if (lcm.keySet.forall(gcdKeys(_))) {
      helper(gcd.toList, List.empty) match {
        case Some(l) => {
          l.reduce(_ * _)
        }
        case None => 0
      }
    }
    else {
      0
    }


  }

  def getTotalX(a: Array[Int], b: Array[Int]): Int = {
    val primes = genPrimes(Stream.from(2))

    val primeFactors1 = primeFactorize(a.toList, primes)
    val lcm = leastCommonMultiplePrimes(primeFactors1)

    val primeFactors2 = primeFactorize(b.toList, primes)
    val gcd = greatestCommonDivisorFactors(primeFactors2)

    findNumbers(lcm, gcd)

  }

  def main(args : Array[String]) : Unit = {
    val firstLine = readLine()
    val array1 = readLine().split(" ").map(_.trim.toInt)
    val array2 = readLine().split(" ").map(_.trim.toInt)


    println(getTotalX(array1, array2))


  }

}
