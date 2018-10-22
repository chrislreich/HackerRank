package FunctionalProgramming

/**
  * Created by creich on 2/21/18.
  */
import scala.collection.immutable.HashMap
object PentagonalDots {

  def go(inputList : List[Int]) : List[BigInt] = {
    val maxQuery = inputList.max

    def next(i : Int, map : HashMap[Int, BigInt]) : HashMap[Int, BigInt] = {
      if (i > maxQuery) {
        map
      }
      else {
        val previousState = map.get(i - 1) match {case Some(x) => x}
        val nextState : BigInt = previousState + (2 * i) + (i - 2)
        next(i + 1, map + (i -> nextState))
      }
    }

    val queryMap = next(2, new HashMap[Int, BigInt]() + (1 -> BigInt(1)))

    inputList.map(x => queryMap.get(x) match {case Some(k) => k})
  }



  def main(args : Array[String]) : Unit = {
    val t = readInt()
    val inputList = (for (i <- 0 until t) yield readInt()).toList

    val resultList = go(inputList)

    println(resultList.mkString("\n"))
  }

}
