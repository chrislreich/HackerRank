package CrackingCoding

/**
  * Created by creich on 5/30/18.
  */
import scala.collection.immutable.HashMap
object CC_BFS_ShortestPath {

  def constructGraph(edges : List[(Int, Int)], map : HashMap[Int, Set[Int]]) : HashMap[Int, Set[Int]] = {
    edges match {
      case x :: xs => {
        val prev_1 = map.getOrElse(x._1, Set[Int]())
        val tempMap = map + (x._1 -> (prev_1 + x._2))
        val prev_2 = map.getOrElse(x._2, Set[Int]())
        constructGraph(xs, tempMap + (x._2 -> (prev_2 + x._1)))
      }
      case _ => {
        map
      }
    }
  }


  def bfs(graph : HashMap[Int, Set[Int]], start_node : Int) : HashMap[Int, Int] = {
    def helper(currentLevel : List[Int], nextLevel : List[Int], depth : Int, seenSet : Set[Int], retMap : HashMap[Int, Int]) : HashMap[Int, Int] = {
      currentLevel match {
        case x :: xs => {
          val filteredChildren = graph.getOrElse(x, Set[Int]()).filter(!seenSet(_)).toList
          helper(xs, filteredChildren ++ nextLevel, depth, seenSet ++ filteredChildren, retMap + (x -> depth))
        }
        case _ => {
          if (nextLevel isEmpty) {
            retMap
          }
          else {
            helper(nextLevel, List[Int](), depth + 6, seenSet, retMap)
          }
        }
      }
    }

    helper(List(start_node), List[Int](), 0, Set[Int](start_node), HashMap[Int, Int]())
  }

  def run = {
    val n = readInt()
    for (i <- 0 until n) {
      val firstLine = readLine().split(" ").map(_ toInt)
      val n = firstLine(0)
      val m = firstLine(1)
      val edges : List[(Int, Int)] = (for (j <- 0 until m) yield readLine().split(" ").map(_ toInt)).map(x => (x(0), x(1))).toList
      val s = readInt()
      val graph = constructGraph(edges, HashMap[Int, Set[Int]]())
      val distanceMap = bfs(graph, s)

      val distanceList = for (t <- 1 until n + 1; if (t != s)) yield distanceMap.getOrElse(t, -1)
      println(distanceList.mkString(" "))
    }

  }

  def main(args : Array[String]) : Unit = {
    run
  }
}
