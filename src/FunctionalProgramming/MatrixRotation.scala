package FunctionalProgramming


import scala.collection.immutable.HashMap
object MatrixRotation {



  def generateRotations(m : Int, n: Int, r : Int) : HashMap[(Int, Int), (Int, Int)] = {
    val yHalf = (m / 2)
    val xHalf = (n / 2)



    def makeList(startPoint : (Int, Int)) : List[(Int, Int)] = {
      val xMin = startPoint._1
      val yMin = startPoint._2
      val xMax = n - xMin + 1
      val yMax = m - yMin + 1

      val topRow = for(i <- xMin until xMax + 1) yield (i, yMin)
      val rightColumn = for(i <- yMin until yMax + 1) yield (xMax, i)
      val bottomRow = for(i <- xMax until xMin - 1 by -1) yield (i, yMax)
      val leftColumn = for(i <- yMax until yMin - 1 by -1) yield (xMin, i)

      val retList = (topRow.init ++ rightColumn.init ++ bottomRow.init ++ leftColumn.init).toList

      retList

    }

    def generateLists(curStartPoint : (Int, Int), retSet : Set[List[(Int, Int)]]) : Set[List[(Int, Int)]] = {
      val curX = curStartPoint._1
      val curY = curStartPoint._2
      if (curX > xHalf || curY > yHalf) {
        retSet
      }
      else {
        generateLists((curX + 1, curY + 1), Set[List[(Int, Int)]](makeList(curStartPoint)) ++ retSet)
      }
    }

    def zipRotation(input : List[(Int, Int)]) : List[((Int, Int), (Int, Int))] = {
      val offset = r % input.length
      val splitLists = input.splitAt(offset)
      val newTail = splitLists._1
      val newHead = splitLists._2

      val rotatedList = newHead ++ newTail

      input zip rotatedList

    }

    val listSet = generateLists((1,1), Set[List[(Int, Int)]]())




    var retMap = new HashMap[(Int, Int), (Int, Int)]()


    for (l <- listSet) {
      val zippedLists = zipRotation(l)
      for (e <- zippedLists) {
        val originalAlignment = e._1
        val newAlignment = e._2
        retMap = retMap + (originalAlignment -> newAlignment)
      }

    }

    retMap

  }



  def main(args : Array[String]) : Unit = {
    val firstLine = readLine()

    val splitArr = firstLine.split(" ")
    val m = splitArr(0).toInt
    val n = splitArr(1).toInt
    val r = splitArr(2).toInt

    val inputMap = scala.collection.mutable.HashMap[(Int, Int), Int]()

    for (i <- 1 until m + 1) {
      val nextLine = readLine()
      val rowArray = nextLine.split(" ")
      for (j <- 1 until n + 1){
        inputMap += ((j, i) -> rowArray(j - 1).toInt)
      }
    }


    val rotations = generateRotations(m, n, r)



    for (i <- 1 until m + 1;
         j <- 1 until n + 1) {
      val newAlignment = rotations.getOrElse((j, i), (0, 0))
      val newValue = inputMap.getOrElse(newAlignment, -1)

      if (j == n)
        println(newValue)
      else
        print(newValue.toString + " ")
    }


  }

}
