package FunctionalProgramming

/**
  * Created by creich on 4/12/18.
  */
object CaptainPrime {

  val primes = genPrimes(10000000).toSet
  val noZeroes = primes.filterNot(x => x.toString.contains('0'))

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def genPrimes(n : Int) : List[Int] = {
    val root = scala.math.sqrt(n).toInt
    val arr = Array.fill[Boolean](n + 1)(true)
    def nextPrime(cur : Int) : Int = {
      if (arr(cur)) {
        cur
      }
      else {
        nextPrime(cur + 1)
      }
    }
    def helper(curPrime : Int, retList : List[Int]) : List[Int] = {
     if (curPrime > root) {
     val restOfPrimes = for (i <- curPrime until n; if (arr(i))) yield i
       retList.reverse ++ restOfPrimes
     }
     else {
       for (i <- (curPrime * curPrime) until n by curPrime) {
         arr(i) = false
       }
       helper(nextPrime(curPrime + 1), curPrime :: retList)
     }
    }
    helper(2, Nil)
  }


  def fitsLeft(input : Int) : Boolean = {
    def helper(s : String) : Boolean = {
      if (s isEmpty) {
        true
      }
      else {
        if (primes(s.toInt)) {
          helper(s.tail)
        }
        else {
          false
        }
      }
    }
    helper(input.toString.tail)
  }

  def fitsRight(input : Int) : Boolean = {
    def helper(s : String) : Boolean = {
      if (s isEmpty) {
        true
      }
      else {
        if (primes(s.toInt)) {
          helper(s.init)
        }
        else {
          false
        }
      }
    }
    helper(input.toString.init)
  }

  def processInt(num : Int) : String = {
    if (primes(num) && noZeroes(num)) {
      (fitsLeft(num), fitsRight(num)) match {
        case (true, true) => {
          "CENTRAL"
        }
        case (true, false) => {
          "LEFT"
        }
        case (false, true) => {
          "RIGHT"
        }
        case (false, false) => {
          "DEAD"
        }
      }
    }
    else {
      "DEAD"
    }
  }


  def run : Unit = {
    val t = readInt()
    for (i <- 0 until t) {
      val current = readInt()
      println(processInt(current))
    }
  }


  def main(args : Array[String]) : Unit = {
    run
  }

}
