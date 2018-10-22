package FunctionalProgramming

/**
  * Created by creich on 1/23/18.
  */

import scala.collection.immutable.HashMap
object GCDPrimes {

  def findGCD(list : List[List[Int]]) : List[Int] = {
    def helper(input: List[List[Int]], currentGCD: HashMap[Int, Int]): HashMap[Int, Int] = {
      input match {
        case x :: xs => {
          val iter = x.sliding(2, 2)
          var newMap = HashMap[Int, Int]()
          for (e <- iter) {
            val prime = e.head
            val pow = e.last

            val oldVal = currentGCD.getOrElse(prime, -1)
            if (oldVal != -1) {
              if (pow < oldVal)
                newMap = newMap + (prime -> pow)
              else
                newMap = newMap + (prime -> oldVal)
            }
          }
          helper(xs, newMap)
        }
        case _ => currentGCD
      }
    }

    def formatResult(m : HashMap[Int, Int]) : List[Int] = {

      def helper(s : List[Int], resList : List[Int]) : List[Int] = {
        if (s.isEmpty)
          resList
        else {
          val h = s.head
          val t = s.tail
          val result = m.getOrElse(h, -1)
          helper(t, result :: (h :: resList))
        }
      }
      helper(m.keySet.toList.sortWith((a,b) => a < b), List[Int]()).reverse
    }

    var initialMap = HashMap[Int, Int]()
    val head = list.head
    val tail = list.tail

    for (e <- head.sliding(2,2)) {
      initialMap = initialMap + (e.head -> e.last)
    }

    val resultMap = helper(tail, initialMap)

    formatResult(resultMap)


  }


  def main(args : Array[String]) : Unit = {
    val q = readInt()

    var inputList = List[List[Int]]()

    for(i <- 0 until q) {
      val nextLine = readLine().split(" ").toList.map(_.toInt)
      inputList  = nextLine :: inputList
    }

    val gcd = findGCD(inputList)
    println(gcd.mkString(" "))
  }

}
