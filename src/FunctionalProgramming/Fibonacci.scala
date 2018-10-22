package FunctionalProgramming

/**
  * Created by creich on 2/23/18.
  */
import scala.collection.immutable.HashMap
object Fibonacci {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  def fibonacci(commands : List[Int]) : List[BigInt] = {
    val maxCommand = commands.max

    val initialMap = new HashMap[Int, BigInt]()
    val map0 = initialMap + (0 -> BigInt(0))
    val map1 = map0 + (1 -> BigInt(1))

    def genFibs(n : Int, n1 : BigInt, n2 : BigInt, map : HashMap[Int, BigInt]) : HashMap[Int, BigInt] = {
      if (n > maxCommand) {
        map
      }
      else {
        genFibs(n + 1, n1 + n2, n1, map + (n -> (n1 + n2)))
      }
    }

    val queryMap = genFibs(2, 1, 0, map1)

    for (c <- commands) yield queryMap.get(c) match {case Some(x) => x}
  }





  def run : Unit = {
    val m = BigInt(100000007)
    val t = readInt()
    val inputCommands = (for (i <- 0 until t) yield readInt()).toList

    val result = fibonacci(inputCommands)

    println(result.map(_ % m).mkString("\n"))
  }




  def main(args : Array[String]) : Unit = {
    run
  }

}
