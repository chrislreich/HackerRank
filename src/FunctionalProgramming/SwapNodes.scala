package FunctionalProgramming


object SwapNodes {

  class TreeNode(val index : Int, var leftChild : TreeNode = null, var rightChild : TreeNode = null)
  {
    def setLeftChild(t : TreeNode) : Unit = {
      leftChild = t
    }

    def setRightChild(t : TreeNode) : Unit = {
      rightChild = t
    }



    override def toString: String = {

      val left = if (leftChild == null) {
        "NULL"
      }
      else {
        leftChild.index.toString
      }


      val right = if (rightChild == null) {
        "NULL"
      }

      else {
        rightChild.index.toString
      }


      "\nINDEX: " + index.toString + "\nLEFT: " + left + "\nRIGHT: " + right
    }

  }

  def copyTree(t : TreeNode) : TreeNode = {
    val newCurrentNode = new TreeNode(t.index)
    if (t.leftChild != null) {
      newCurrentNode.setLeftChild(copyTree(t.leftChild))
    }
    if (t.rightChild != null) {
      newCurrentNode.setRightChild(copyTree(t.rightChild))
    }
    newCurrentNode
  }

  def swapBranches(t : TreeNode, curDepth : Int, k : Int) : TreeNode = {
    if (curDepth % k == 0) {
      val tempLeft = t.leftChild
      val tempRight = t.rightChild

      val newRight = if (tempLeft != null) {
        swapBranches(tempLeft, curDepth + 1, k)
      }
      else {
        null
      }
      val newLeft = if (tempRight != null) {
        swapBranches(tempRight, curDepth + 1, k)
      }
      else {
        null
      }

      new TreeNode(t.index, newLeft, newRight)
    }
    else {
      val newCurNode = new TreeNode(t.index)
      if (t.leftChild != null) {
        newCurNode.setLeftChild(swapBranches(t.leftChild, curDepth + 1, k))
      }

      if (t.rightChild != null) {
        newCurNode.setRightChild(swapBranches(t.rightChild, curDepth + 1, k))
      }
      newCurNode

    }
  }


  def inOrderTraversal(curNode : TreeNode, outputList : List[Int]) : List[Int] = {
    if (curNode == null) {
      outputList
    }
    else {
      val leftList = inOrderTraversal(curNode.leftChild, outputList)
      val thisAdded = curNode.index :: leftList
      inOrderTraversal(curNode.rightChild, thisAdded)
    }
  }


  def main(args : Array[String]) : Unit = {
    val n = readInt()

    val nodeMap = scala.collection.mutable.HashMap[Int, TreeNode]()

    val rootNode  = new TreeNode(1)
    nodeMap += (1 -> rootNode)

    for (i <- 1 until n + 1) {
      val nextLine = readLine()
      val splitArr = nextLine.split(" ")
      val leftInt = splitArr(0).toInt
      val rightInt = splitArr(1).toInt

      val curNode = nodeMap.getOrElse(i, new TreeNode(0))

      if (leftInt != -1) {
        val newLeft = new TreeNode(leftInt)
        nodeMap += (leftInt -> newLeft)
        curNode.setLeftChild(newLeft)
      }

      if (rightInt != -1) {
        val newRight = new TreeNode(rightInt)
        nodeMap += (rightInt -> newRight)
        curNode.setRightChild(newRight)
      }

      nodeMap += (i -> curNode)

    }

    val t = readInt()

    var currentRoot = rootNode

    for (j <- 0 until t) {
      val k = readInt()
      currentRoot = swapBranches(currentRoot,1,k)
      val printList = inOrderTraversal(currentRoot, List[Int]())
      println(printList.reverse.mkString(" "))
    }





  }



}
