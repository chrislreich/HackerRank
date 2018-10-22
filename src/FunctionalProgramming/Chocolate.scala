package FunctionalProgramming

/**
  * Created by creich on 2/22/18.
  */
import scala.collection.immutable.HashMap
object Chocolate {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  def mkMap : HashMap[(Int, Int, Int), Boolean] = {




    def generateFinalWinningStates : HashMap[(Int, Int, Int), Boolean] = {
      var map = new HashMap[(Int, Int, Int), Boolean]()
      // vertical
      map = map + ((1,1,1) -> true)
      map = map + ((1,1,0) -> true)
      //horizontal
      for (i <- 2 until 4) {
        map = map + ((i, 0, 0) -> true)
      }
      map + ((1, 0, 0) -> false)
    }


    def generateNextStates(tuple : (Int, Int, Int)) : List[(Int, Int, Int)] = {
      val topRow = for (i <- tuple._3 - 1 until -1 by -1) yield (tuple._1, tuple._2, i)
      val middleRow = for (i <- tuple._2 - 1 until -1 by -1)
        yield
          if (i > tuple._3) {
            (tuple._1, i, tuple._3)
          }
          else {
            (tuple._1, i, i)
          }

      val bottomRow = for (i <- tuple._1 - 1 until 0 by -1)
        yield
          (i > tuple._3, i > tuple._2) match {
            case (true, true) => (i, tuple._2, tuple._3)
            case (true, false) => (i, i, tuple._3)
            case (false, false) => (i, i, i)
          }
      (topRow ++ middleRow ++ bottomRow).toList
    }


    def processNextStates(list : List[(Int, Int, Int)], map : HashMap[(Int, Int, Int), Boolean], retList : List[Boolean]) : (List[Boolean], HashMap[(Int, Int, Int), Boolean]) = {
      list match {
        case x :: xs => {
          val current = map.get(x) match {
            case Some(x) => (x, map)
            case None => populateMap(x, map)
          }
          processNextStates(xs, current._2, current._1 :: retList)
        }
        case _ => (retList, map)
      }
    }

    def populateMap(tuple : (Int, Int, Int), map : HashMap[(Int, Int, Int), Boolean]) : (Boolean, HashMap[(Int, Int, Int), Boolean]) = {
      val nextStateLocations = generateNextStates(tuple)
      val nextStateValues = processNextStates(nextStateLocations, map, Nil)
      val retBooleanVal = nextStateValues._1.exists(_ == false)
      (retBooleanVal, nextStateValues._2 + (tuple -> retBooleanVal))
    }

    val initialMap = generateFinalWinningStates

    populateMap((25,25,25), initialMap)._2

  }







  def processInput : Unit = {
    val t = readInt()
    val commandList = (for(i <- 0 until t) yield readLine()).toList.map(x => x.split(" ")).map(y => (y(0).toInt, y(1).toInt, y(2).toInt))

    val queryMap = mkMap

    for (c <- commandList) {
      val result = queryMap.get(c) match {case Some(x) => x}
      if (result){
        println("WIN")
      }
      else {
        println("LOSE")
      }
    }


  }


  def main(args : Array[String]) : Unit = {
    processInput

  }

}
