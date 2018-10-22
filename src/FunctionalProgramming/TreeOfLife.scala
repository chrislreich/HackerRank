package FunctionalProgramming

import scala.collection.immutable.HashMap

/**
  * Created by creich on 1/22/18.
  */
object TreeOfLife {

  class ComputeTree(val rule : Int, val serialTree : String) {

    class TreeNode(val on_? : Boolean, var parentNode : TreeNode = null, var leftChild : TreeNode = null, var rightChild : TreeNode = null) {
      def check : String = {
        if (on_?)
          "1"
        else
          "0"
      }
      def setParent(t : TreeNode) : Unit = {
        parentNode = t
      }
      def setLeftChild(t : TreeNode) : Unit = {
        leftChild = t
      }
      def setRightChild(t : TreeNode) : Unit = {
        rightChild = t
      }

      override def toString: String = {
        def helper(t : TreeNode) : String = {
          if (t == null) {
            "NULL"
          }
          else {
            t.toString
          }
        }

        "\nVALUE: " + on_? + "\nPARENT: "+ helper(parentNode)
      }

    }

    def findSubTreeStringIndex(input : String) : Int = {

      def helper(str : String, parenCnt : Int, indexCnt : Int) : Int = {
        if (parenCnt == 0)
          indexCnt
        else {
          str.head match {
            case '(' => helper(str.tail, parenCnt + 1, indexCnt + 1)
            case ')' => helper(str.tail, parenCnt - 1, indexCnt + 1)
            case _ => helper(str.tail, parenCnt, indexCnt + 1)
          }
        }
      }
      helper(input.tail, 1, 1)
    }

    def parseTree(curString : String) : TreeNode = {

      if (curString.length == 1) {
        curString match {
          case "." => new TreeNode(false)
          case "X" => new TreeNode(true)
        }
      }
      else {

        val remaining = curString.tail
        val leftChar = remaining.head
        val leftLeaf_? = leftChar match {
          case '.' => true
          case 'X' => true
          case _ => false
        }

        val splitIndex = if (leftLeaf_?) {
          1
        }
        else {
          findSubTreeStringIndex(remaining)
        }

        val leftSplit = remaining.splitAt(splitIndex)

        val leftSubTreeString = leftSplit._1
        val remList = leftSplit._2.tail

        val curNodeValue = remList.head match {
          case 'X' => true
          case '.' => false
          case _ => println("SERIOUS ERROR"); println(remList.head); true
        }
        val rightRemaining = remList.drop(2)

        val rightChar = rightRemaining.head
        val rightLeaf_? = rightChar match {
          case '.' => true
          case 'X' => true
          case _ => false
        }
        val rightSubtreeString = rightRemaining.reverse.tail.reverse


        val curNode = new TreeNode(curNodeValue, null, null, null)

        val leftSubtree = if (leftLeaf_?) {
          leftChar match {
            case '.' => new TreeNode(false, curNode)
            case 'X' => new TreeNode(true, curNode)
          }
        }
        else {
          parseTree(leftSubTreeString)
        }

        leftSubtree.setParent(curNode)


        val rightSubtree = if (rightLeaf_?) {
          rightChar match {
            case '.' => new TreeNode(false, curNode)
            case 'X' => new TreeNode(true, curNode)
          }
        }
        else {
          parseTree(rightSubtreeString)
        }

        rightSubtree.setParent(curNode)

        curNode.setLeftChild(leftSubtree)
        curNode.setRightChild(rightSubtree)

        curNode
      }

    }

    private def createRuleMap : HashMap[String, Char] = {
      val binaryString = rule.toBinaryString.reverse.padTo(16, '0')

      def neighborhoodBinary(i : Int) : String = {
        i.toBinaryString.reverse.padTo(4, '0').reverse
      }

      def addToMap(map : HashMap[String, Char], i : Int) : HashMap[String, Char] = {
        map + ((neighborhoodBinary(i) -> binaryString(i)))
      }

      (0 to 15).foldLeft(HashMap[String, Char]())(addToMap)
    }

    private def checkNodeState(t : TreeNode) : String = {
      if (t == null){
        "0"
      }
      else {
        t.check
      }
    }

    private def computeNextState(t : TreeNode) : TreeNode = {
      val checkString = checkNodeState(t.parentNode) + checkNodeState(t.leftChild) + t.check + checkNodeState(t.rightChild)
      val newState = ruleMap.getOrElse(checkString, '0') match {
        case '0' => false
        case '1' => true
      }

      val curNode = new TreeNode(newState)

      if (t.leftChild != null) {
        val tempLeft = computeNextState(t.leftChild)
        tempLeft.setParent(curNode)
        curNode.setLeftChild(tempLeft)
      }

      if (t.rightChild != null) {
        val tempRight = computeNextState(t.rightChild)
        tempRight.setParent(curNode)
        curNode.setRightChild(tempRight)
      }
      curNode
    }


    private def retrieveRequestedState(input : Int) : TreeNode = {
      val requestedStateNumber = programCounter + input
      if (requestedStateNumber <= maxSize && requestedStateNumber >= 0) {
        programCounter = requestedStateNumber
        stateMap.getOrElse(programCounter, new TreeNode(false))
      }
      else {
       for (i <- maxSize + 1 until requestedStateNumber + 1) {
         val oldState = stateMap.getOrElse(i - 1, new TreeNode(false))
         val newState = computeNextState(oldState)
         stateMap = stateMap + (i -> newState)
       }
        programCounter = requestedStateNumber
        maxSize = requestedStateNumber
        stateMap.getOrElse(maxSize, new TreeNode(false))
      }
    }

    private def parsePath(p : String, t : TreeNode) : String = {
      if (p.isEmpty)
        t.check
      else {
        p.head match {
          case '<' => parsePath(p.tail, t.leftChild)
          case '>' => parsePath(p.tail, t.rightChild)
        }
      }
    }

    def findNodeValue(stateIncrement : Int, path : String) : Char = {
      val neededState = retrieveRequestedState(stateIncrement)
      val cleanedPath = path.tail.reverse.tail.reverse
      val nodeState = parsePath(cleanedPath, neededState)

      nodeState match {
        case "0" => '.'
        case "1" => 'X'
      }

    }


    private var programCounter = 0
    private var maxSize = 0
    private val ruleMap = createRuleMap
    private val initialTree = parseTree(serialTree)

    private var stateMap = HashMap[Int, TreeNode](0 -> initialTree)


  }

  def main(args : Array[String]) : Unit = {

    /*

    val test1 = new ComputeTree(42354,  "((. X (. . .)) . (X . (. X X)))")

    println(test1.findNodeValue(0 ,"[]"))
    println(test1.findNodeValue(2, "[><]"))
    println(test1.findNodeValue(0, "[><]"))
    println(test1.findNodeValue(0, "[<>]"))
    println(test1.findNodeValue(1, "[><]"))
    println(test1.findNodeValue(0, "[<>]"))

    */


    val intRule = readInt()
    val serializedTree = readLine()
    val stateMachine = new ComputeTree(intRule, serializedTree)
    val c = readInt()

    for (j <- 0 until c) {
      val nextLine = readLine()
      val splitArr = nextLine.split(" ")
      val incrementor = splitArr(0).toInt
      val path = splitArr(1)
      println(stateMachine.findNodeValue(incrementor, path))

    }



  }

}
