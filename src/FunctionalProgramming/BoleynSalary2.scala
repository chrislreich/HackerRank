package FunctionalProgramming

import scala.collection.mutable.HashSet

/**
  * Created by creich on 3/30/18.
  */
object BoleynSalary2 {

  var childArr : Array[Employee] = Array.empty
  var parentArr : Array[Option[Int]] = Array.empty


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  class BSTNode(val position : Int, val salary : Int, val leftChild : Option[BSTNode] = None, val rightChild : Option[BSTNode] = None, val leftSize : Int = 0, val rightSize : Int = 0)

  def insertNode(nodeOption : Option[BSTNode], t : (Int, Int)) : BSTNode = {
    nodeOption match {
      case None => {
        new BSTNode(t._1, t._2)
      }
      case Some(n) => {
        if (t._2 < n.salary) {
          val newLeftNode = insertNode(n.leftChild, t)
          new BSTNode(n.position, n.salary, Some(newLeftNode), n.rightChild, n.leftSize + 1, n.rightSize)
        }
        else {
          val newRightNode = insertNode(n.rightChild, t)
          new BSTNode(n.position, n.salary, n.leftChild, Some(newRightNode), n.leftSize, n.rightSize + 1)
        }
      }
    }
  }

  def treeToList(t : BSTNode, retList : List[(Int, Int)]) : List[(Int, Int)] = {
    (t.leftChild, t.rightChild) match {
      case (Some(l), Some(r)) => {
        val fromLeft = treeToList(l, (t.position, t.salary) :: retList)
        treeToList(r, fromLeft)
      }
      case (Some(l), None) => {
        treeToList(l, (t.position, t.salary) :: retList)
      }
      case (None, Some(r)) => {
        treeToList(r, (t.position, t.salary) :: retList)
      }
      case (None, None) => {
        (t.position, t.salary) :: retList
      }
    }
  }

  def insertList(l : List[(Int, Int)], n : BSTNode) : BSTNode = {
    l match {
      case x :: xs => {
        val newNode = insertNode(Some(n), x)
        insertList(xs, newNode)
      }
      case _ => {
        n
      }
    }
  }

  def mergeTrees(l : BSTNode, r : BSTNode) : BSTNode = {

    val leftSize = l.leftSize + l.rightSize + 1
    val rightSize = r.leftSize + r.rightSize + 1

    if (leftSize < rightSize) {
      val leftAsList = treeToList(l, Nil)
      insertList(leftAsList, r)
    }
    else {
      val rightAsList = treeToList(r, Nil)
      insertList(rightAsList, l)
    }
  }

  def nodeLookup(node : Option[BSTNode], position : Int) : BSTNode = {
    node match {
      case Some(n) => {
        val curNum = n.leftSize + 1
        if (position == curNum) {
          n
        }
        else {
          if (position < curNum) {
            nodeLookup(n.leftChild, position)
          }
          else {
            nodeLookup(n.rightChild, position - curNum)
          }
        }
      }
      case None => {
        new BSTNode(-1, -1)
      }
    }
  }


  class Employee(var salary : Int = 0, val subordinates : HashSet[Int] = new HashSet[Int](), var bst : Option[BSTNode] = None)

  def findLeaves : List[Int] = {
    (for (i <- 1 until childArr.length; if (childArr(i).subordinates isEmpty)) yield i).toList
  }

  def propagateLeaf(l : Int, node : Option[BSTNode]) : Unit = {
    (childArr(l).bst, node) match {
      case (None, None) => {
      }
      case (Some(n), None) => {
      }
      case (None, Some(n)) => {
        childArr(l).bst = node
      }
      case (Some(n1), Some(n2)) => {
        childArr(l).bst = Some(mergeTrees(n1, n2))
      }
    }
    if (childArr(l).subordinates isEmpty) {
      parentArr(l) match {
        case Some(p) => {
          childArr(p).subordinates -= l
          propagateLeaf(p, Some(insertNode(childArr(l).bst, (l, childArr(l).salary))))
        }
        case None => {

        }
      }
    }
  }

  def run : Unit = {
    val firstLine = readLine().split(" ").map(_ toInt)
    val n = firstLine(0)
    val q = firstLine(1)

    childArr = Array.fill[Employee](n + 1)(new Employee())
    parentArr = Array.fill[Option[Int]](n + 1)(None)

    for (i <- 1 until n) {
      val line = readLine().split(" ").map(_ toInt)
      val supervisor = childArr(line(1))
      supervisor.subordinates += line(0)
      parentArr(line(0)) = Some(line(1))

    }

    val salaries = readLine().split(" ").map(_ toInt)

    for (i <- 0 until n) {
      childArr(i + 1).salary = salaries(i)
    }

    val leaves = findLeaves

    time {

      for (leaf <- leaves) {
        propagateLeaf(leaf, None)
      }
    }

    var d = 0

    for (i <- 0 until q) {
      val line = readLine().split(" ").map(_ toInt)
      val result = nodeLookup(childArr(line(0) + d).bst, line(1))
      d = result.position
      println(d)
    }


  }



  def main(args : Array[String]) : Unit = {
    run

  }

}
