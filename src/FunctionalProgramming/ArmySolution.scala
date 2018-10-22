package FunctionalProgramming

/**
  * Created by creich on 3/16/18.
  */
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.StdIn.readLine

object Solution {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }
  var armies = new HashMap[Int, Army]()

  def findArmy(id: Int): Army = {
    if (armies.contains(id)) {
      return armies(id)
    } else {
      val army = new Army()
      armies += (id -> army)
      return army;
    }
  }

  def main(args: Array[String]) {
    val arr = readLine.split(" ").map(_.toInt)
    var q = arr(1)
    time {
      while (q > 0) {
        val p = readLine.split(" ").map(_.toInt)
        val army = findArmy(p(1))
        p(0) match {
          case 1 => println(army.findStrongest())
          case 2 => army.strongestDied()
          case 3 => army.recruit(p(2))
          case 4 => {
            var secondArmyId = p(2)
            army.merge(findArmy(secondArmyId))
            armies.remove(secondArmyId)
          }
        }
        q -= 1
      }
    }
  }
}

class Army() {
  private var soldiers = new ArrayBuffer[Int]()
  def findStrongest(): Int = {
    return soldiers.reduceLeft(_ max _)
  }
  def strongestDied() {
    soldiers -= findStrongest()
  }
  def recruit(c: Int) {
    soldiers += c
  }
  def merge(army: Army) {
    soldiers ++= army.getSoldiers()
  }
  def getSoldiers() = {
    soldiers
  }
}
