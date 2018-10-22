package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_HashTable_IceCreamFlavors {

  def makeTable(flavors : List[(Long, Int)], retMap : Map[Long, List[Int]]) : Map[Long, List[Int]] = {
    flavors match {
      case x :: xs => {
        val prevList = retMap.getOrElse(x._1, List[Int]())
        makeTable(xs, retMap + (x._1 -> (x._2 :: prevList)))
      }
      case _ => {
        retMap
      }
    }
  }

  def findPair(pairs : List[(Long, List[Int])], map : Map[Long, List[Int]], money : Long) : Option[List[Int]] = {
    pairs match {
      case x :: xs => {
        val remaining = money - x._1
        if (remaining == x._1) {
          if (x._2.length >= 2) {
            Some(x._2.take(2))
          }
          else {
            findPair(xs, map, money)
          }
        }
        else {
          map.get(remaining) match {
            case Some(r) => {
              Some(List(x._2.head, r.head))
            }
            case None => {
              findPair(xs, map, money)
            }
          }
        }
      }
      case _ => {
        None
      }
    }
  }

  def run = {
    val t = readInt()
    for (i <- 0 until t) {
      val m = readInt().toLong
      val n = readInt()
      val flavorCosts = readLine().split(" ").map(_.trim.toLong).toList

      val zippedIndex = (0L :: flavorCosts).zipWithIndex.tail

      val mappedFlavors = makeTable(zippedIndex, Map[Long, List[Int]]())

      val unsortedAnswer = findPair(mappedFlavors.toList, mappedFlavors, m) match {case Some(a) => a; case None => List[Int]()}

      println(unsortedAnswer.sorted.mkString(" "))

    }
  }

  def main(args: Array[String]) : Unit = {
    run
  }

}
