package FunctionalProgramming

/**
  * Created by creich on 1/15/18.
  */


import scala.collection.immutable.HashMap


object Crossword {


  class WordSpace(val id : Int, val isVertical : Boolean, val startPoint : (Int, Int), val len : Int, val initialIntersections : Set[(Int, Int)])
  {
    var isFilled : Boolean = false
    var charSpaces : HashMap[(Int, Int), Char] = HashMap[(Int, Int), Char]()
    var edges : Set[(WordSpace, (Int, Int))] = Set[(WordSpace, (Int, Int))]()

    def getCharFromCoordinate(coordinate : (Int, Int)) : Char = {
      charSpaces.getOrElse(coordinate, '!')
    }

    private def assignCoordinates(str : String) : Unit = {
      val column = startPoint._1
      val row = startPoint._2
      if (isVertical) {
        for (i <- 0 until len) {
          charSpaces = charSpaces + ((column, row + i) -> str(i))
        }
      }
      else {
        for (i <- 0 until len) {
          charSpaces = charSpaces + ((column + i, row) -> str(i))
        }
      }
    }

    def cleanCharSpaces : Unit = charSpaces = HashMap[(Int, Int), Char]()

    def fits_?(str : String) : Boolean = {
      if (str.length == len) {
        assignCoordinates(str)
        if (charOverlap_?) {
          cleanCharSpaces
          return true
        }
        else {
          cleanCharSpaces
          return false
        }
      }
      else
        return false
    }


    private def charOverlap_? : Boolean = {
      for (e <- edges) {
        val obj = e._1
        val intersection = e._2
        if (obj.isFilled) {
          val c1 = obj.getCharFromCoordinate(intersection)
          val c2 = getCharFromCoordinate((intersection))
          if (c1 != c2)
            return false
        }
      }
      return true
    }

    def fillSpace(string : String) : Unit = {
      isFilled = true
      assignCoordinates(string)
    }

    override def toString: String = {
      "ID: " + id + ", Verticality: " + isVertical + ", StartPoint: " + startPoint.toString
    }

    def mkString : String = {
      var retString = ""
      for (k <- charSpaces) {
        retString += k + "\n"
      }
      retString
    }
  }





  def findWordSpaces(board : Array[Array[Boolean]]) : Set[WordSpace] = {
    val retSet = scala.collection.mutable.Set[WordSpace]()
    var cnt = 1


    def checkBoard(p :(Int, Int)) : Boolean = {
      if (p._1 >= 10 || p._1 < 0 || p._2 >= 10 || p._2 < 0) {
        return false
      }
      if (board(p._2)(p._1) == false)
        true
      else
        false
    }



    def isHorizontalWord_?(tuple : (Int, Int)) : Boolean = {
      checkBoard((tuple._1 + 1, tuple._2)) ||  checkBoard(tuple._1 - 1, tuple._2)
    }

    def isVerticalWord_?(tuple :(Int, Int)) : Boolean = {
      checkBoard((tuple._1, tuple._2 + 1)) ||  checkBoard(tuple._1, tuple._2 - 1)
    }

    def startPoint_?(tuple : (Int, Int), isHorizontal : Boolean) : Boolean = {
      val curX = tuple._1
      val curY = tuple._2
      if (isHorizontal)
        curX == 0 || !checkBoard((curX - 1), curY)
      else
        curY == 0 || !checkBoard((curX, curY - 1))
    }




    def processWordStartPoint(pt : (Int, Int), isHorizontal : Boolean) : WordSpace = {
      val edges = scala.collection.mutable.Set[(Int, Int)]()

      def helper(inputPt : (Int, Int), count : Int) : Int = {
        if (isHorizontal) {
          if (checkBoard(inputPt)) {
            if (isVerticalWord_?(inputPt)) {
              edges += inputPt
            }
            helper((inputPt._1 + 1, inputPt._2), count + 1)
          }
          else
            count
        }
        else {
          if (checkBoard(inputPt)) {
            if (isHorizontalWord_?(inputPt)) {
              edges += inputPt
            }
            helper((inputPt._1, inputPt._2 + 1), count + 1)
          }
          else
            count
        }
      }

      val spaceLen = helper(pt, 0)
      val retSpace = new WordSpace(cnt, !isHorizontal, pt, spaceLen, edges.toSet)
      cnt += 1
      retSpace
    }

    for(i <- 0 until 10;
        j <- 0 until 10) {
      val curPt = (j, i)
      if (checkBoard(curPt)) {
        if (isHorizontalWord_?(curPt) && startPoint_?(curPt, true))
          retSet += processWordStartPoint(curPt, true)
        if (isVerticalWord_?(curPt) && startPoint_?(curPt, false))
          retSet += processWordStartPoint(curPt, false)
      }
    }

    retSet.toSet

  }



  def createEdges(inputSet : Set[WordSpace]) : Set[WordSpace] = {
    val resultSet = scala.collection.mutable.Set[WordSpace]()
    val copyOfSet = inputSet

    for (ws: WordSpace <- inputSet) {
      val currentEdgeList = scala.collection.mutable.Set[(WordSpace, (Int, Int))]()
      val curID = ws.id
      val curIntersections = ws.initialIntersections
      val setWithoutCurrentItem = copyOfSet.filter(x => x.id != curID)
      for (d <- setWithoutCurrentItem) {
        val matchIntersection = d.initialIntersections & curIntersections
        if (matchIntersection.size == 1) {
          currentEdgeList += ((d, matchIntersection.head))
        }
        else {
          if (matchIntersection.size > 1) {
            println("ERROR: Intersection Set is > 1")
          }
        }

        }
      ws.edges = currentEdgeList.toSet
      resultSet.add(ws)
      }
    resultSet.toSet
    }


  def placeWords(wordList : Set[String], openSpaceList : List[WordSpace], filledSpaces : List[WordSpace]) : Option[List[WordSpace]] = {
    if (wordList.isEmpty)
      return Some(filledSpaces)
    else {
      val curSpace = openSpaceList.head
      for (w <- wordList) {
        if (curSpace.fits_?(w)) {
          curSpace.fillSpace(w)
          placeWords(wordList.filter(_ != w), openSpaceList.tail, curSpace :: filledSpaces) match {
            case Some(x) => return Some(x)
            case _ => {
              curSpace.cleanCharSpaces
              curSpace.isFilled = false
            }
          }
        }
      }
    }
    return None
  }


  def printCharBoard(board : Array[Array[Char]]) : Unit = {
    for (i <- 0 until 10;
         j <- 0 until 10) {
      var curChar = board(i)(j)
      if (curChar == '\0')
        curChar = '+'
      if (j == 9) {
        println(curChar)
      }
      else {
        print(curChar)
      }
    }
  }


  def createBoard() : Array[Array[Boolean]] = {
    val board = new Array[Array[Boolean]](10)
    for (i <- 0 until 10) {
      board(i) = new Array[Boolean](10)
    }
    board
  }


  def main(args : Array[String]) : Unit = {
    val sc = new java.util.Scanner(System.in)
    val board = createBoard()

    for (i <- 0 until 10){
      val newLine = sc.nextLine()
      for (j <- 0 until 10) {
        val curChar = newLine(j)
        if (curChar == '+')
          board(i)(j) = true
        else
          board(i)(j) = false
      }
    }

    val inputWords = sc.nextLine().split(";").toSet

    val spaceSet = findWordSpaces(board)
    val edgeSet = createEdges(spaceSet)

    val placedSet = placeWords(inputWords, edgeSet.toList, List.empty) match {
      case Some(x) => x
      case None => Set.empty
    }

    val charBoard = new Array[Array[Char]](10)

    for (i <- 0 until 10)
      charBoard(i) = new Array[Char](10)



    for (p <- placedSet) {
      for (t <- p.charSpaces) {
        val curCoordinate = t._1
        val curX = curCoordinate._1
        val curY = curCoordinate._2
        val curChar = t._2
        charBoard(curY)(curX) = curChar

      }
    }

    printCharBoard(charBoard)

  }

}
