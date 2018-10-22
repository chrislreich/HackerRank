package FunctionalProgramming

/**
  * Created by creich on 2/27/18.
  */
import scala.collection.immutable.HashMap
object SherlockMaze4 {

  val m = 1000000007L
  var kPrime = 1000000

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  //case class SquareState(aboveMap : HashMap[Int, BigInt], rightMap : HashMap[Int, BigInt])

  def coalesceFromRight(s : (HashMap[Int, Long], HashMap[Int, Long])) : HashMap[Int, Long] = {
    val newAbove = s._1.map(x => (x._1 + 1, x._2)).filter(_._1 < 92)
    s._2.merged(newAbove)({ case ((k,v1),(_, v2)) => (k, (v1 + v2) % m)})
  }

  def coalesceFromAbove(s : (HashMap[Int, Long], HashMap[Int, Long])) : HashMap[Int, Long] = {
    val newRight = s._2.map(x => (x._1 + 1, x._2)).filter(_._1 < 92)
    s._1.merged(newRight)({ case ((k,v1),(_, v2)) => (k, (v1 + v2) % m)})
  }


  def processInput(list : List[(Int, Int, Int)]) : Unit = {
    val arr = new Array[Array[(HashMap[Int, Long], HashMap[Int, Long])]](101)
    def initializeArray : Unit = {
      for (i2 <- 1 until 101) {
        arr(i2) = new Array[(HashMap[Int, Long], HashMap[Int, Long])](101)
      }
      for (i <- 2 until 101) {
        arr(i)(1) = (HashMap[Int, Long](), HashMap[Int, Long]() + (0 -> 1L))
        arr(1)(i) = (HashMap[Int, Long]() + (0 -> 1L), HashMap[Int, Long]())
      }
      arr(1)(1) -> (HashMap[Int, Long](), HashMap[Int, Long]())
    }

    def stateMachine(m : Int, n : Int) : Unit = {
      if (arr(m)(n) == null) {
        val newRight = if (m == 1) {
          HashMap[Int, Long]()
        }
        else {
          stateMachine(m - 1, n)
          coalesceFromRight(arr(m - 1)(n))

        }
        val newAbove = if (n == 1) {
          HashMap[Int, Long]()
        }
        else {
          stateMachine(m, n - 1)
          coalesceFromAbove(arr(m)(n - 1))
        }
        arr(m)(n) = (newAbove, newRight)
      }
      else {

      }
    }

    def query(tuple : (Int, Int, Int)) : Long = {
      if ((tuple._1, tuple._2) == (1,1)) {
        1L
      }
      else {
        val x = arr(tuple._1)(tuple._2)
        val above: Long = x._1.filter(x => x._1 <= tuple._3).values.sum % m
        val right: Long = x._2.filter(x => x._1 <= tuple._3).values.sum % m
        (above + right) % m
        }

    }

    initializeArray
    kPrime = list.maxBy(x => x._3)._3

    for (w <- list)  {
      stateMachine(w._1, w._2)
      println(query(w))
    }


  }



  def run1 : Unit = {
    val t = readInt()
    val inputList = (for (i <- 0 until t) yield readLine.split(" ")).toList.map(x => (x(0).toInt, x(1).toInt, x(2).toInt))

    time {
      processInput(inputList)
    }

  }



  def main(args : Array[String]) : Unit = {
    run1
  }


}
