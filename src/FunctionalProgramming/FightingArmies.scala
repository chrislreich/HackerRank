package FunctionalProgramming

/**
  * Created by creich on 2/20/18.
  */

import scala.collection.mutable.{HashMap, PriorityQueue}


object FightingArmies {

  val map = new HashMap[Int, PriorityQueue[Int]]()


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def nextStateOne(i : Int) : HashMap[Int, PriorityQueue[Int]] = {
    val retVal = map.get(i) match {
      case Some(x) => x.head
    }
    println(retVal)
    map
  }

  def nextStateTwo(i : Int, map : HashMap[Int, PriorityQueue[Int]]) : HashMap[Int, PriorityQueue[Int]] = {
    map.get(i) match {
      case Some(x) => {
        x.dequeue()
        map
      }
    }
  }

  def nextStateThree(i : Int, j : Int, map : HashMap[Int, PriorityQueue[Int]]) : HashMap[Int, PriorityQueue[Int]] = {
    val pq = map.getOrElse(i, new PriorityQueue[Int]())
    pq += j
    map + (i -> pq)
    map
  }

  def nextStateFour(i : Int, j : Int, map : HashMap[Int, PriorityQueue[Int]]) : HashMap[Int, PriorityQueue[Int]] = {
    val pqi = map.get(i) match {case Some(x) => x}
    val pqj = map.get(j) match {case Some(x) => x}
    pqi ++= pqj
    map - j
  }


  def nextState(command : String) : Unit = {
    val commandArr = command.split(" ")
    val i = commandArr(1).toInt

    commandArr(0).toInt match {
      case 1 => {
        val retVal = map.get(i) match {
          case Some(x) => x.head
        }
        println(retVal)
      }
      case 2 => {
        map.get(i) match {
          case Some(x) => {
            x.dequeue()
          }
        }
      }
      case 3 => {
        val c = commandArr(2).toInt
        val pq = map.getOrElse(i, new PriorityQueue[Int]())
        pq += c
        map += (i -> pq)
      }
      case 4 => {
        val j = commandArr(2).toInt
        val pqi = map.get(i) match {case Some(x) => x}
        val pqj = map.get(j) match {case Some(x) => x}
        pqi ++= pqj
        map -= j
      }
    }
  }



  def stdInSource : Unit = {
    val firstLine = scala.io.StdIn.readLine().split(" ")
    val n = firstLine(0).toInt
    val q = firstLine(1).toInt

time {
  for (i <- 0 until q) {
    nextState(scala.io.StdIn.readLine())
  }
}



  }





  def main(args : Array[String]) : Unit = {
    stdInSource
  }

}
