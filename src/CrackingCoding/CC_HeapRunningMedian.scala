package CrackingCoding

/**
  * Created by creich on 5/24/18.
  */
object CC_HeapRunningMedian {


  class Node(val value : Int, val leftSize : Int = 0, val rightSize : Int = 0, val leftChild : Option[Node] = None, val rightChild : Option[Node] = None)

  class Median(val median : Int, val maxHeap : Option[Node], val minHeap : Option[Node], val maxHeapSize : Int, val minHeapSize : Int)

  def insertMaxHeap(curNode : Option[Node], v : Int) : Node = {
    curNode match {
      case Some(n) => {
        (n.leftSize <= n.rightSize, v > n.value) match {
          case (true, true) => {
            val newLeftNode = insertMaxHeap(n.leftChild, n.value)
            new Node(v, n.leftSize + 1, n.rightSize, Some(newLeftNode), n.rightChild)
          }
          case (false, true) => {
            val newRightNode = insertMaxHeap(n.rightChild, n.value)
            new Node(v, n.leftSize, n.rightSize + 1, n.leftChild, Some(newRightNode))
          }
          case (true, false) => {
            val newLeftNode = insertMaxHeap(n.leftChild, v)
            new Node(n.value, n.leftSize + 1, n.rightSize, Some(newLeftNode), n.rightChild)
          }
          case (false, false) => {
            val newRightNode = insertMaxHeap(n.rightChild, v)
            new Node(n.value, n.leftSize, n.rightSize + 1, n.leftChild, Some(newRightNode))
          }
        }
      }
      case None => new Node(v)
    }
  }

  def swapMaxHeap(node : Node, v : Int) : Node = {
    (node.leftChild, node.rightChild) match {
      case (Some(l), Some(r)) => {
        if (v > l.value && v > r.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          if (l.value >= r.value) {
            val newLeftNode = swapMaxHeap(l, v)
            new Node(l.value, node.leftSize, node.rightSize, Some(newLeftNode), node.rightChild)
          }
          else {
            val newRightNode = swapMaxHeap(r, v)
            new Node(r.value, node.leftSize, node.rightSize, node.leftChild, Some(newRightNode))
          }
        }
      }
      case (Some(l), None) => {
        if (v > l.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          val newLeftNode = swapMaxHeap(l, v)
          new Node(l.value, node.leftSize, node.rightSize, Some(newLeftNode), node.rightChild)
        }
      }
      case (None, Some(r)) => {
        if (v > r.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          val newRightNode = swapMaxHeap(r, v)
          new Node(r.value, node.leftSize, node.rightSize, node.leftChild, Some(newRightNode))
        }
      }
      case (None, None) => {
        new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
      }
    }
  }

  def insertMinHeap(curNode : Option[Node], v : Int) : Node = {
    curNode match {
      case Some(n) => {
        (n.leftSize <= n.rightSize, v < n.value) match {
          case (true, true) => {
            val newLeftNode = insertMinHeap(n.leftChild, n.value)
            new Node(v, n.leftSize + 1, n.rightSize, Some(newLeftNode), n.rightChild)
          }
          case (false, true) => {
            val newRightNode = insertMinHeap(n.rightChild, n.value)
            new Node(v, n.leftSize, n.rightSize + 1, n.leftChild, Some(newRightNode))
          }
          case (true, false) => {
            val newLeftNode = insertMinHeap(n.leftChild, v)
            new Node(n.value, n.leftSize + 1, n.rightSize, Some(newLeftNode), n.rightChild)
          }
          case (false, false) => {
            val newRightNode = insertMinHeap(n.rightChild, v)
            new Node(n.value, n.leftSize, n.rightSize + 1, n.leftChild, Some(newRightNode))
          }
        }
      }
      case None => new Node(v)
    }
  }

  def swapMinHeap(node : Node, v : Int) : Node = {
    (node.leftChild, node.rightChild) match {
      case (Some(l), Some(r)) => {
        if (v < l.value && v < r.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          if (l.value <= r.value) {
            val newLeftNode = swapMinHeap(l, v)
            new Node(l.value, node.leftSize, node.rightSize, Some(newLeftNode), node.rightChild)
          }
          else {
            val newRightNode = swapMinHeap(r, v)
            new Node(r.value, node.leftSize, node.rightSize, node.leftChild, Some(newRightNode))
          }
        }
      }
      case (Some(l), None) => {
        if (v < l.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          val newLeftNode = swapMinHeap(l, v)
          new Node(l.value, node.leftSize, node.rightSize, Some(newLeftNode), node.rightChild)
        }
      }
      case (None, Some(r)) => {
        if (v < r.value) {
          new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
        }
        else {
          val newRightNode = swapMinHeap(r, v)
          new Node(r.value, node.leftSize, node.rightSize, node.leftChild, Some(newRightNode))
        }
      }
      case (None, None) => {
        new Node(v, node.leftSize, node.rightSize, node.leftChild, node.rightChild)
      }
    }
  }

  def insertNewNumber(v : Int, m : Median) : Median = {
    (v > m.median, m.minHeapSize == m.maxHeapSize) match {
      case (true, true) => {
        val tuple = m.minHeap match {
          case Some(h) => {
            if (v <= h.value) {
              (Some(h), v)
            }
            else {
              (Some(swapMinHeap(h, v)), h.value)
            }
          }
          case None => {
            (None, v)
          }
        }
        val newMinHeap = tuple._1
        val newMedian = tuple._2
        val newMaxHeap = insertMaxHeap(m.maxHeap, m.median)
        new Median(newMedian, Some(newMaxHeap), newMinHeap, m.maxHeapSize + 1, m.minHeapSize)
      }
      case (true, false) => {
        val newMinHeap = insertMinHeap(m.minHeap, v)
        new Median(m.median, m.maxHeap, Some(newMinHeap), m.maxHeapSize, m.minHeapSize + 1)
      }
      case (false, true) => {
        val newMaxHeap = insertMaxHeap(m.maxHeap, v)
        new Median(m.median, Some(newMaxHeap), m.minHeap, m.maxHeapSize + 1, m.minHeapSize)
      }
      case (false, false) => {
        val tuple = m.maxHeap match {
          case Some(h) => {
            if (v >= h.value) {
              (Some(h), v)
            }
            else {
              (Some(swapMaxHeap(h, v)), h.value)
            }
          }
          case None => {
            (None, v)
          }
        }
        val newMaxHeap = tuple._1
        val newMedian = tuple._2
        val newMinHeap = insertMinHeap(m.minHeap, m.median)
        new Median(newMedian, newMaxHeap, Some(newMinHeap), m.maxHeapSize, m.minHeapSize + 1)
      }
    }
  }

  def formatMedian(m : Median) : String = {
    if (m.maxHeapSize == m.minHeapSize) {
      val median = m.median
      f"$median%.1f"
    }
    else {
     // println("two medians")
      val m1 = m.median
      val m2 = m.maxHeap match {case Some(x) => x.value}
      val sum = m1 + m2
      //println(sum)
      val median = sum / 2.0
      //println(median)
      f"$median%.1f"
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    var currentMedian = new Median(readInt(), None, None, 0, 0)
    println(formatMedian(currentMedian))



    for (i <- 1 until n) {
      val aItem = stdin.readLine.trim.toInt
      currentMedian = insertNewNumber(aItem, currentMedian)
      println(formatMedian(currentMedian))
    }
  }

}
