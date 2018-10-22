package FunctionalProgramming

/**
  * Created by creich on 1/10/18.
  * NOTE: I mixed up the traditional Cartesian coordinate notation of (X,Y)
  * And instead used (Y,X) for the tuples on accident
  */
object SierpinskiTriangles {

  def findLeftVertex(curVertex : (Int, Int), curHeight : Int) : (Int, Int) = {
    (curVertex._1 - (curHeight / 2), ((curHeight / 2) + curVertex._2 ))

  }
  def findRightVertex(curVertex : (Int, Int), curHeight : Int) : (Int, Int) = {
    (curVertex._1 + (curHeight / 2), ((curHeight / 2) + curVertex._2))
  }

  def findVertices(curVertex : (Int, Int), height : Int, depth : Int) : Set[(Int, Int)] = {
    if (depth == 0) Set(curVertex)
    else findVertices(curVertex, height/2, depth - 1) ++ findVertices(findLeftVertex(curVertex, height), height / 2, depth - 1) ++ findVertices(findRightVertex(curVertex, height), height / 2, depth - 1)

  }

  def trianglePoints(vertex : (Int, Int), height : Int) : IndexedSeq[(Int, Int)] = {
    for (i <- 0 until height;
         j <- vertex._1 - i until vertex._1 + i + 1) yield (vertex._2 + i, j)
  }

  def convertVerticesToTriangles(vertexSet : Set[(Int, Int)], height : Int) : List[(Int, Int)] = {
    vertexSet.foldLeft(List[(Int, Int)]())((l, v) => l ++ trianglePoints(v, height).toList)
  }

  def generateIndices() : IndexedSeq[(Int, Int)] = {
    for (i <- 1 until 33;
         j <- 1 until 64) yield (i, j)
  }

  def drawTriangles(n: Int) {
      for (v <- generateIndices) {
      if (convertVerticesToTriangles(findVertices((32, 1), 32, n), (32 / scala.math.pow(2, n).toInt)).contains(v)) {
        print("1")
      }
      else print("_")
      if (v._2 == 63) print("\n")
    }

  }


  def main(args: Array[String]) {
    drawTriangles(readInt())
  }

}
