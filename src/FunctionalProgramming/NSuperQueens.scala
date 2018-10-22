package FunctionalProgramming

import scala.collection.immutable.{HashSet, Seq}


/**
  * Created by creich on 1/17/18.
  */
object NSuperQueens {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0) / 10e8) + " s")
    result
  }





  def findNSuperQueens(n : Int, tp : Int) : HashSet[HashSet[(Int, Int)]] = {
    val propagateLookup  = new scala.collection.mutable.HashMap[(Int, Int), Set[(Int, Int)]]()

    def generateAttackPoints(x : Int, y : Int) : Set[(Int, Int)] = {



      def generateQueenMoves : Seq[(Int, Int)] = {

        def diagonalHelper(xIncrementor : Int, yIncrementor : Int, xLimit : Int, yLimit : Int) : Seq[(Int, Int)] = {
          def helper(curX : Int, curY : Int, acc : List[(Int, Int)]) : Seq[(Int, Int)] = {
            if (curX == xLimit || curY == yLimit)
              acc
            else
              helper(curX + xIncrementor, curY + yIncrementor, (curX, curY) :: acc)
          }
          helper(x + xIncrementor, y + yIncrementor, List.empty)
        }

        def queenUp : Seq[(Int, Int)] = {
          for(i <- y + 1 until n + 1)
            yield (x, i)
        }

        def queenDown : Seq[(Int, Int)] = {
          for(i <- y - 1 until 0 by -1)
            yield (x, i)
        }

        def queenLeft : Seq[(Int, Int)] = {
          for(i <- x - 1 until 0 by -1)
            yield (i, y)
        }

        def queenRight  : Seq[(Int, Int)] = {
          for (i <- x + 1 until n + 1)
            yield (i, y)
        }

        def queenDiagonals : Seq[(Int, Int)] = {
          diagonalHelper(1,1,n + 1,n + 1) ++ diagonalHelper(1, -1, n + 1, 0) ++ diagonalHelper(-1, -1, 0, 0) ++ diagonalHelper(-1, 1, 0, n + 1)
        }

        queenUp ++ queenDown ++ queenLeft ++ queenRight ++ queenDiagonals
      }

      def generateKnightMoves : Seq[(Int, Int)] = {
        val twoUp = List((x + 1, y + 2), (x - 1, y + 2))
        val oneUp = List((x + 2, y + 1), (x - 2, y + 1))
        val oneDown = List((x + 2, y - 1), (x - 2, y - 1))
        val twoDown = List((x + 1, y - 2), (x - 1, y - 2))

        (twoUp ++ twoDown ++ oneUp ++ oneDown).filterNot(a => {val x1 = a._1; val y1 = a._2; (x1 < 1 || x1 > n || y1 < 0 || y1 > n)})
      }

      (generateKnightMoves ++ generateQueenMoves).toSet
    }

    def checkPosition(column : Int, row : Int, alreadyPlaced : HashSet[(Int, Int)], eliminatedPoints : HashSet[(Int, Int)]) : HashSet[Option[HashSet[(Int, Int)]]] = {
      if (row > n)
        new HashSet[Option[HashSet[(Int, Int)]]]() + Some(alreadyPlaced)
      else {
       if (column > n)
         new HashSet[Option[HashSet[(Int, Int)]]]() + None
       else {
         if (!eliminatedPoints.contains((column, row))) {
           checkPosition(1, row + 1, alreadyPlaced ++ HashSet[(Int, Int)]((column, row)) , eliminatedPoints ++ generateAttackPoints(column, row)) ++ checkPosition(column + 1, row, alreadyPlaced, eliminatedPoints)
         }
         else
           checkPosition(column + 1, row, alreadyPlaced, eliminatedPoints)
       }
      }
    }



    def checkPosition2(count : Int, availablePoints : HashSet[(Int, Int)], alreadyPlaced : HashSet[(Int, Int)]) : HashSet[Option[HashSet[(Int, Int)]]] = {
      if (count == n)
        new HashSet[Option[HashSet[(Int, Int)]]]() + Some(alreadyPlaced)
      else {
        if (availablePoints.isEmpty)
          new HashSet[Option[HashSet[(Int, Int)]]]() + None
        else {
          val splitLists = availablePoints.splitAt(1)
          val head = splitLists._1.head
          val tail = splitLists._2
          checkPosition2(count + 1, tail -- generateAttackPoints(head._1, head._2), alreadyPlaced + head)
        }
      }
    }

    def checkPosition3(row : Int, availablePoints : HashSet[(Int, Int)], placedPoints : HashSet[(Int, Int)]) : HashSet[Option[HashSet[(Int, Int)]]] = {
      if (row > n)
        HashSet[Option[HashSet[(Int, Int)]]](Some(placedPoints))
      else {
        val rowPoints = availablePoints.filter(x => x._2 == row)
        if (rowPoints.isEmpty)
          HashSet[Option[HashSet[(Int, Int)]]](None)
        else {
          var retSet = new HashSet[Option[HashSet[(Int, Int)]]]()
          for (p <- rowPoints)
            retSet = retSet ++ checkPosition3(row + 1, (availablePoints - p) -- propagateLookup.getOrElse(p, HashSet[(Int, Int)]()), placedPoints + p)
          retSet
        }
      }
    }

    def genAvailablePoints : HashSet[(Int, Int)] = {
      new HashSet[(Int, Int)]() ++ (for (i <- 1 until n + 1;
            j <- 1 until n + 1)
        yield (j, i)).toSet
    }

    def fillPropagationTable : Unit = {
      for (i <- 1 until n + 1;
           j <- 1 until n + 1) {
        val attackPoints = generateAttackPoints(j, i)
        propagateLookup += (j, i) -> attackPoints
      }
    }

    tp match {
      case 1 => {
        val setOptions: HashSet[Option[HashSet[(Int, Int)]]] = (1 to n).foldLeft(new HashSet[Option[HashSet[(Int, Int)]]]())((b, x) => checkPosition(x, 1, new HashSet[(Int, Int)](), new HashSet[(Int, Int)]()) ++ b)
        val validOptions = setOptions.filter(_ nonEmpty)
        var retSet = new HashSet[HashSet[(Int, Int)]]()
        for (vo <- validOptions) {
          vo match {
            case Some(x) => retSet = retSet + x
          }
        }
        retSet
      }
      case 2 => {
        val startPoints = genAvailablePoints
        val setOptions = (1 to n).foldLeft(new HashSet[Option[HashSet[(Int, Int)]]]())((b, x) => checkPosition2(1, (startPoints - ((x, 1))) -- generateAttackPoints(x, 1), HashSet[(Int, Int)]((x, 1))) ++ b)
        val validOptions = setOptions.filter(_ nonEmpty)
        var retSet = new HashSet[HashSet[(Int, Int)]]()
        for (vo <- validOptions) {
          vo match {
            case Some(x) => retSet = retSet + x
          }
        }
        retSet
      }
      case 3 => {
        val startPoints = genAvailablePoints
        fillPropagationTable
        val setOptions = checkPosition3(1, startPoints, new HashSet[(Int, Int)]())

        val validOptions = setOptions.filter(_ nonEmpty)
        var retSet = new HashSet[HashSet[(Int, Int)]]()
        for (vo <- validOptions) {
          vo match {
            case Some(x) => retSet = retSet + x
          }
        }
        retSet

      }
    }




  }

  def checkLists(answer : List[(Int, Int)], resultSet : Set[List[(Int, Int)]]) : Boolean = {
    def helper(l : List[(Int, Int)], a : List[(Int, Int)]) : Boolean = {
      a match {
        case x :: xs => {
          if (l.contains(x))
            helper(l, xs)
          else
            false
        }
        case _ => true
      }
    }

    for (d <- resultSet) {
      if (helper(d, answer))
        return true
    }
    return false
  }



  def printBoard(queens : Set[(Int, Int)], limit : Int) : Unit = {
    for (i <- limit until 0  by -1;
         j <- 1 until limit + 1) {
      if (j == limit) {
        if (queens.contains((j, i)))
          println('X')
        else
          println('-')
      }
      else {
        if (queens.contains((j, i)))
          print("X ")
        else
          print("- ")
      }

    }

  }


  def main(args: Array[String]) : Unit = {


    val n = readInt()


    //val res1 = time {findNSuperQueens(n ,1)}


/*
    for (d <- res1) {
      println("new list")
      println(d)
      printBoard(d, n)
    }
*/



   // println(res1.size)

    val res2 = time{findNSuperQueens(n, 3)}
    println(res2.size)










  }

}
