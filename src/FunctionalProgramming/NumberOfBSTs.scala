package FunctionalProgramming

/**
  * Created by creich on 2/21/18.
  */
import scala.collection.immutable.HashMap
object NumberOfBSTs {


  def go(inputList : List[Int]) : List[BigInt] = {
    val maxCommand = inputList.max

    def nextState(i : Int, map : HashMap[Int, BigInt]) :  HashMap[Int, BigInt] = {
     if (i > maxCommand) {
       map
     }
     else {
       val oneLess = i - 1
       val oneLessState = map.get(oneLess) match {case Some(x) => x}
       val middle = i / 2

       val half: BigInt =  ((for (j <- 1 until middle) yield {
         val left = map.get(oneLess - j) match {case Some(x) => x}
         val right = map.get(j) match {case Some(x) => x}
         left * right
       }).sum + oneLessState) * 2

       val extra : BigInt = if (i % 2 == 1) {
         val l = map.get(oneLess/2) match {case Some(x) => x}
         l * l
       }
       else {
         0
       }
       nextState(i + 1, map + (i -> (half + extra)))
     }
    }

    val finishedMap = nextState(2, new HashMap[Int, BigInt]() + (1 -> BigInt(1)))

    for (n <- inputList) yield finishedMap.get(n) match {case Some(x) => x}
  }






  def main(args: Array[String]) : Unit = {
    val n = readInt()
    val m = BigInt(100000007)

    val commands = (for (i <- 0 until n) yield readInt()).toList
    val retList = go(commands)

    for (r <- retList) {
      println(r % m)
    }
  }
}
