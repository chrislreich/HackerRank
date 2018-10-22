package FunctionalProgramming

/**
  * Created by creich on 2/19/18.
  */
import scala.collection.immutable.HashMap
object MaxFunctions {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  case class LinearFunction(val yIntercept : Int, val slope : Long, val index : Int)



  def makeLinearFunctionObjects(l : List[(Int, Long)]) : List[LinearFunction] = {
    ((1,1L) :: l).zipWithIndex.tail.map(x => new LinearFunction(x._1._1, x._1._2, x._2))
  }


  def stateMachine(list : List[LinearFunction], commandList : List[Int]) : List[Int] = {
    val maxCommand = commandList.max
    val initialMax = list.maxBy(x => (x.yIntercept, x.index))
    val initialFiltered = list.filter(_.slope > initialMax.slope)

    val finalState = list.map(x => (x, x.yIntercept + (x.slope * maxCommand)))
    val finalMax = finalState.maxBy(x => (x._2, x._1.index))._1
    val initialFilteredTwo = finalMax :: initialFiltered.filter(_.yIntercept > finalMax.yIntercept)

    def findXIntercept(current : LinearFunction, candidate : LinearFunction) : Double = {
      val constants = current.yIntercept - candidate.yIntercept
      val variables = candidate.slope - current.slope
      constants / variables.toDouble
    }


    def findNextLeader(currentMax : LinearFunction, candidateList : List[LinearFunction]) : (Int, LinearFunction, List[LinearFunction]) = {
      val nextMaxStateTemp = candidateList.map(x => (findXIntercept(currentMax, x), x)).minBy(x => x._1)
      val nextMaxState = if (scala.math.ceil(nextMaxStateTemp._1) == nextMaxStateTemp._1 && nextMaxStateTemp._2.index < currentMax.index) {
        nextMaxStateTemp._1.toInt + 1
      }
      else {
        scala.math.ceil(nextMaxStateTemp._1).toInt
      }
      val nextMax = candidateList.map(x => (nextMaxState * x.slope + x.yIntercept, x)).maxBy(j => (j._1, j._2.index))._2
      val filteredList = candidateList.filter(_.slope > nextMax.slope)
      (nextMaxState, nextMax, filteredList)
    }


    def iterate(curMax : LinearFunction, candidates : List[LinearFunction], start : Int, map : HashMap[Int, Int]) : HashMap[Int, Int] = {
      if (curMax == finalMax) {
        var finalMap = map
        for (i <- start until maxCommand + 1) {
          finalMap = finalMap + (i -> curMax.index)
        }
        finalMap
      }
      else {
        val nextLeader = findNextLeader(curMax, candidates)
        var nextMap = map
        for (i <- start until nextLeader._1) {
          nextMap = nextMap + (i -> curMax.index)
        }
        iterate(nextLeader._2, nextLeader._3, nextLeader._1, nextMap)
      }
    }

    def processCommands(input : List[Int], map : HashMap[Int, Int], retList : List[Int]) : List[Int] = {
      input match {
        case x :: xs => {
          val lookup = map.get(x) match {case Some(x) => x}
          processCommands(xs, map, lookup :: retList)
        }
        case _ => retList
      }
    }


    val lookupMap = iterate(initialMax, initialFilteredTwo, 0, new HashMap[Int, Int]())


    processCommands(commandList, lookupMap, List[Int]())


  }


  def testResult(result : List[Int], commands : List[Int], q : Int) : Unit = {

    println("please supply answer")

    val answers = for (i <- 0 until q) yield readInt()
    val answerList = answers.toList

    val zipped = answerList zip result

    val zippedQueryNumber: List[((Int, Int), Int)] = zipped zip commands

    val matched = zippedQueryNumber.takeWhile(x => (x._1._2 == x._1._1))

    println("Matched: " + matched.length)
    println("First Unmatched : " + zippedQueryNumber.drop(matched.length).headOption)

  }


  def main(args : Array[String]) : Unit = {
    val line = readLine().split(" ")
    val n = line(0).toInt
    val q = line(1).toInt
    val initialList = readLine().split(" ").toList.map(_.toInt)
    val slopes = readLine().split(" ").toList.map(_.toLong)

    var commandList = List[Int]()
    for (i <- 0 until q) {
      commandList = readInt() :: commandList
    }

    val functions = initialList zip slopes
    val functionObjects = makeLinearFunctionObjects(functions)
    val res = stateMachine(functionObjects, commandList)
    println(res.mkString("\n"))

    //testResult(res, commandList.reverse, q)
  }
}
