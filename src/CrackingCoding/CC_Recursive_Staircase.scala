package CrackingCoding

/**
  * Created by creich on 6/1/18.
  */
object CC_Recursive_Staircase {

  def recurse(stairsLeft : Int, map : Map[Int, Int]) : (Int, Map[Int, Int]) = {
    if (stairsLeft < 0) {
      (0, map)
    }
    else {
      if (stairsLeft == 0) {
        (1, map)
      }
      else {
        val minusOne = map.get(stairsLeft - 1) match {
          case None => recurse(stairsLeft - 1, map)
          case Some(x) => (x, map)
        }

        val minusTwo = minusOne._2.get(stairsLeft - 2) match {
          case None => recurse(stairsLeft - 2, minusOne._2)
          case Some(x) => (x, minusOne._2)
        }

        val minusThree = minusTwo._2.get(stairsLeft - 3) match {
          case None => recurse(stairsLeft - 3, minusTwo._2)
          case Some(x) => (x, minusTwo._2)
        }

        val sum = minusOne._1 + minusTwo._1 + minusThree._1
        (sum, minusThree._2 + (stairsLeft -> sum))
      }
    }
  }

  def run = {
    val s = readInt()
    for (i <- 0 until s) {
      println(recurse(readInt(), Map[Int, Int]())._1)
    }

  }

  def main(args : Array[String]) : Unit = {
    run
  }

}
