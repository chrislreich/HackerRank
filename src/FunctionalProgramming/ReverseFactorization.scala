package FunctionalProgramming

/**
  * Created by creich on 2/23/18.
  */
object ReverseFactorization {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }



  def factors(n : BigInt, set : List[Int]) : Option[List[BigInt]] = {
    def helper(currentSet : List[Int], currentNum : BigInt, retList : List[BigInt]) : Option[List[BigInt]] = {
      currentSet match {
        case x :: xs => {
          if (currentNum == 1) {
            Some(currentNum :: retList)
          }
          else {
            if (currentNum % x == 0) {
              helper(currentSet, currentNum / x, currentNum :: retList)
            }
            else {
              helper(xs, currentNum, retList)
            }
          }
        }
        case _ => None
      }
    }


    helper(set, n, Nil)
  }




  def run : Unit = {
    val firstLine = readLine().split(" ")
    val n = BigInt(firstLine(0))
    val set = readLine().split(" ").toList.map(_.toInt)
    val sortedSet = set.sortBy(x => -x)

    factors(n, sortedSet) match {
      case Some(x) => {
        println(x.mkString(" "))
      }
      case None => println("-1")
    }

  }




  def main(args : Array[String]) : Unit = {
    run
  }

}
