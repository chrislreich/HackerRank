package CrackingCoding

/**
  * Created by creich on 5/30/18.
  */
object CC_DFS_GridConnectedComponents {


  def dfs(n : Int, m: Int, array : Array[Array[Int]]) : Map[(Int, Int), Int] = {

    def genMoves(i : Int, j : Int) : List[(Int, Int)] = {
      val allMoves = List((i + 1, j + 1), (i + 1, j), (i + 1, j - 1), (i, j + 1), (i, j - 1), (i - 1, j + 1), (i - 1, j), (i - 1, j - 1))
      allMoves.filter(x => x._1 >= 0 && x._1 < n && x._2 >= 0 && x._2 < m)
    }

    def component(curTuple : (Int, Int), seen : Set[(Int, Int)], retMap : Map[(Int, Int), Int], componentNumber : Int) : (Set[(Int, Int)], Map[(Int, Int), Int]) = {
      if (array(curTuple._1)(curTuple._2) == 0) {
        (seen + curTuple, retMap)
      }
      else {
        val children = genMoves(curTuple._1, curTuple._2).filter(!seen(_))
        val newSeen = seen + curTuple
        val newMap = retMap + (curTuple -> componentNumber)
        children.foldLeft((newSeen, newMap))((b, x) => component(x, b._1, b._2, componentNumber))
      }
    }

    def findNextComponent(seen : Set[(Int, Int)]) : Option[(Int, Int)] = {
      for (i <- 0 until n) {
        for (j <- 0 until m) {
          if (!seen((i, j))) {
            return Some((i, j))
          }
        }
      }
      None
    }

    def findConnectedComponents(seen : Set[(Int, Int)], map : Map[(Int, Int), Int], currentComponentNumber : Int) : Map[(Int, Int), Int] = {
      findNextComponent(seen) match {
        case None => {
          map
        }
        case Some(t) => {
          val returnValue = component(t, seen, map, currentComponentNumber)
          findConnectedComponents(returnValue._1, returnValue._2, currentComponentNumber + 1)
        }
      }
    }

    findConnectedComponents(Set[(Int, Int)](), Map[(Int, Int), Int](), 1)

  }

  def largestComponent(map : Map[(Int, Int), Int]) : Int = {
    map.groupBy(x => x._2).mapValues(_.size).maxBy(_._2)._2
  }

  def test = {
    val r = scala.util.Random
    val grid = Array.ofDim[Int](20, 20)

    for (i <- 0 until 20) {
      for (j <- 0 until 20) {
        grid(i)(j) = (r.nextInt() % 2)
      }
    }

    val biggest = dfs(20, 20, grid)
    println(largestComponent(biggest))

  }

  def main(args: Array[String]) {

    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    val m = stdin.readLine.trim.toInt

    val grid = Array.ofDim[Int](n, m)

    for (i <- 0 until n) {
      grid(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val connectedComponents = dfs(n, m, grid)

    println(largestComponent(connectedComponents))
  }

}
