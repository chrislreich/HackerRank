package FunctionalProgramming

/**
  * Created by creich on 4/2/18.
  */
object JumpingBunnies {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def genPrimes(n : Stream[Int]) : Stream[Int] = {
    n.head #:: genPrimes(n.tail.filter(_ % n.head != 0))
  }


  def primeFactors(n : Int, allPrimes : Stream[Int]) : List[(Int, Int)] = {
    def helper(current : Int, primes : Stream[Int], retList : List[(Int, Int)]) : List[(Int, Int)] = {
      if (scala.math.sqrt(current).toInt < primes.head) {
        ((current, 1) :: retList).reverse
      }
      else {
        if (current == 1) {
          retList.reverse
        }
        else {
          if (current % primes.head == 0) {
            if (retList.isEmpty) {
              helper(current / primes.head, primes, (primes.head, 1) :: retList)
            }
            else {
              if (primes.head == retList.head._1) {
                helper(current / primes.head, primes, (primes.head, 1 + retList.head._2) :: retList.tail)
              }
              else {
                helper(current / primes.head, primes, (primes.head, 1) :: retList)
              }
            }
          }
          else {
            helper(current, primes.tail, retList)
          }
        }
      }
    }

    helper(n, allPrimes, Nil)
  }

  def coalescePrimes(acc : List[(Int, Int)], element : List[(Int, Int)]) : List[(Int, Int)] = {
    def helper(curAcc : List[(Int, Int)], curElement : List[(Int, Int)], retList : List[(Int, Int)]) : List[(Int, Int)] = {
      (curAcc, curElement) match {
        case (a :: as, e :: es) => {
          if (a._1 == e._1) {
            helper(as, es, (a._1, scala.math.max(a._2, e._2)) :: retList)
          }
          else {
            if (a._1 < e._1) {
              helper(as, curElement, a :: retList)
            }
            else {
              helper(curAcc, es, e :: retList)
            }
          }
        }
        case (a :: as, Nil) => {
          helper(as, curElement, a :: retList)
        }
        case (Nil, e :: es) => {
          helper(curAcc, es, e :: retList)
        }
        case (Nil, Nil) => {
          retList.reverse
        }
      }
    }
    helper(acc, element, Nil)
  }

  def multiplyFactors(input : List[(Int, Int)]) : Long = {
    input.foldLeft(1L)((b, t) => { var curRet = t._1.toLong; for (i <- 0 until t._2 - 1) curRet *= t._1; b * curRet })
  }


  def run : Unit = {
    val n = readInt()

    val bunnies = readLine().split(" ").map(_ toInt)

    val primeList = genPrimes(Stream.from(2))

    val primeFactorized = for (b <- bunnies) yield primeFactors(b, primeList)

    val coalescedFactors = primeFactorized.foldLeft(List[(Int, Int)]())(coalescePrimes)

    val finalVal = multiplyFactors(coalescedFactors)

    println(finalVal)

  }


  def main(args : Array[String]) : Unit = {
    run
  }

}
