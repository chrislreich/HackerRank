package FunctionalProgramming

/**
  * Created by creich on 1/23/18.
  */

object PrisonTransport {

  def findConnectedComponents(inputMap : Map[Int, List[Int]]) : Map[Int, List[Int]] = {
    val componentMap = scala.collection.mutable.Map[Int, Int]()

    def DFS(map : Map[Int, List[Int]], curNode : Int, curList : List[Int], componentCount : Int) : Map[Int, List[Int]] = {
      curList match {
        case x :: xs => {
          if (map.contains(x)) {
            val nextList = map.getOrElse(x, List()).filter(_ != curNode)
            val newMap = DFS(map - (curNode), x, nextList, componentCount)
            DFS(newMap, curNode, xs, componentCount)
          }
          else {
            DFS(map, curNode, xs, componentCount)
          }
        }
        case _ => {
          componentMap += (curNode -> componentCount)
          map - (curNode)
        }
      }
    }

    def connectedComponents(m : Map[Int, List[Int]], componentNumber : Int) : Int = {
      if (m.isEmpty)
        componentNumber
      else {
        val nextComponent = m.head
        connectedComponents(DFS(m, nextComponent._1, nextComponent._2, componentNumber + 1), componentNumber + 1)
      }
    }

    val numberOfConnectedComponents = connectedComponents(inputMap, 0)

    val groupedByComponentNumber: Map[Int, List[Int]] = componentMap.groupBy(_._2).map { case (k,v) => (k -> v.keys.toList)}

    groupedByComponentNumber

  }

  def calculateBusCost(t : (Int, Int), l :  List[Int]) : (Int, Int) = {
    val len = l.length;
    val cost = scala.math.sqrt(len).ceil.toInt
    (t._1 + len, t._2 + cost)
  }

  def main(args : Array[String]) : Unit = {
    val n = readInt()
    val m = readInt()
    val edgeMap = new scala.collection.mutable.HashMap[Int, List[Int]]()
    for (i <- 0 until m) {
      val nextLine = readLine().split(" ")
      val node1 = nextLine(0).toInt
      val node2 = nextLine(1).toInt
      val oldList1 = edgeMap.getOrElse(node1, List[Int]())
      edgeMap += (node1 -> (node2 :: oldList1))
      val oldList2 = edgeMap.getOrElse(node2, List[Int]())
      edgeMap += (node2 -> (node1 :: oldList2))
    }
    val immutableMap : Map[Int, List[Int]] = edgeMap.toMap

    val connectedComponents = findConnectedComponents(immutableMap).values.toList

    val costTuple = connectedComponents.foldLeft((0,0)) (calculateBusCost)

    val num = costTuple._1
    val busCost = costTuple._2

    val finalCost = busCost + (n - num)


    println(finalCost)
  }

}
