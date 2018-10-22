package FunctionalProgramming

/**
  * Created by creich on 2/23/18.
  */
import scala.collection.immutable.HashMap
object DiceState {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  case class DicePosition(val sum : BigInt, val top : Int, val front : Int, val left : Int)
  class FinalDiceState(val max : BigInt, val positionList : Set[DicePosition])


  def newStateFromLeft(d : DicePosition) : DicePosition = {
    val newTop = d.left
    val newFront = d.front
    val newLeft = 7 - d.top
    DicePosition(d.sum + newTop, newTop, newFront, newLeft)
  }

  def newStateFromAbove(d : DicePosition) : DicePosition = {
    val newTop = 7 - d.front
    val newFront = d.top
    val newLeft = d.left
    DicePosition(d.sum + newTop, newTop, newFront, newLeft)
  }

  def newLeftStates(l : Set[DicePosition]) : Set[DicePosition] = {
    l.map(x => newStateFromLeft(x))
  }

  def newAboveStates(l : Set[DicePosition]) : Set[DicePosition] = {
    l.map(x => newStateFromAbove(x))
  }


  def nextDiceState(m : Int, n : Int, map : HashMap[(Int,Int), FinalDiceState]) : (Option[FinalDiceState], HashMap[(Int,Int), FinalDiceState]) = {
    map get((m, n)) match {
      case Some(x) => (Some(x), map)
      case None => {
        val fromLeftResult = if (n > 1) nextDiceState(m, n - 1, map) else (None, map)
        val fromTopResult =  if (m > 1) nextDiceState(m - 1, n, fromLeftResult._2) else (None, fromLeftResult._2)
        val nextMap = fromTopResult._2

        (fromLeftResult._1, fromTopResult._1) match {
          case (Some(l), Some(a)) => {
            val left = newLeftStates(l.positionList)
            val above = newAboveStates(a.positionList)
            val combinedList = left ++ above
            val newMax = combinedList.maxBy(x => x.sum)
            val newFinal = new FinalDiceState(newMax.sum, combinedList)
            (Some(newFinal), nextMap + ((m, n) -> newFinal))
          }
          case (Some(l), None) => {
            val left = newLeftStates(l.positionList)
            val newMax = left.maxBy(x => x.sum)
            val newFinal = new FinalDiceState(newMax.sum, left)
            (Some(newFinal), nextMap + ((m,n) -> newFinal))
          }
          case (None, Some(a)) => {
            val above = newAboveStates(a.positionList)
            val newMax = above.maxBy(x => x.sum)
            val newFinal = new FinalDiceState(newMax.sum, above)
            (Some(newFinal), nextMap + ((m,n) -> newFinal))
          }
          case (None, None) => {
            (None, nextMap)
          }
        }
      }
    }
  }

  def test1 : Unit = {
    def stateString(d : DicePosition) : String = {
      val back = 7 - d.front
      val right = 7 - d.left
      val bottom = 7 - d.top

      val topString = "Top: " + d.top
      val leftString = "Left: " + d.left
      val bottomString = "Bottom: " + bottom
      val rightString = "Right: " + right
      val frontString = "Front: " + d.front
      val backString = "Back: " + back

      val sep = ", "

      topString + sep + leftString + sep + bottomString + sep + rightString + sep + frontString + sep + backString

    }
    def printState(d : DicePosition) : Unit = {
      println("State: (" + stateString(d) + "), Sum: " + d.sum)
    }

    val firstDice = new DicePosition(1, 1, 2, 3)
    var curMap = new HashMap[(Int, Int), FinalDiceState]() + ((1,1) -> new FinalDiceState(1, Set(firstDice)))
    for (i <- 1 until 7; j <- 1 until 7) {
      val result = nextDiceState(i, j, curMap)
      curMap = result._2
      println("Board Position: " + i + ", " + j)
      val resList = result._1 match { case Some(x) => x.positionList}
      for (r <- resList) {
        printState(r)
      }
    }

  }


  def run1 : Unit = {
    val t = readInt()
    val firstDice = new DicePosition(1, 1, 2, 3)
    var curMap = new HashMap[(Int, Int), FinalDiceState]() + ((1,1) -> new FinalDiceState(1, Set(firstDice)))

    for (i <- 0 until t)
      {
        val line = readLine().split(" ")
        val result = nextDiceState(line(0).toInt, line(1).toInt, curMap)
        curMap = result._2
        val resultValue = result._1 match {case Some(x) => x.max}
        println(resultValue)
      }

  }

  def main(args: Array[String]) : Unit = {
    run1
  }

}
