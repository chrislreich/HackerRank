package FunctionalProgramming

/**
  * Created by creich on 3/20/18.
  */

import scala.util.Random

object MessyMedians {

  var lookupArray = new Array[Median](100001)


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }


  class HeapNode(val value : Int, val leftChild : Option[HeapNode] = None, val rightChild : Option[HeapNode] = None, val leftSize : Int = 0, val rightSize : Int = 0)


  def maxPullUp(h : HeapNode) : Option[HeapNode] = {
    (h.leftChild, h.rightChild) match {
      case (Some(l), Some(r)) => {
        if (l.value > r.value) {
          val newValue = l.value
          val newLeftChild = maxPullUp(l)
          Some(new HeapNode(newValue, newLeftChild, h.rightChild, h.leftSize - 1, h.rightSize))
        }
        else {
          val newValue = r.value
          val newRightChild = maxPullUp(r)
          Some(new HeapNode(newValue, h.leftChild, newRightChild, h.leftSize, h.rightSize - 1))
        }
      }
      case (Some(l), None) => {
        val newValue = l.value
        val newLeftChild = maxPullUp(l)
        Some(new HeapNode(newValue, newLeftChild, None, h.leftSize - 1, 0))
      }
      case (None, Some(r)) => {
        val newValue = r.value
        val newRightChild = maxPullUp(r)
        Some(new HeapNode(newValue, None, newRightChild, 0, h.rightSize - 1))
      }
      case (None, None) => {
        None
      }
    }
  }

  def maxPushDown(nodeOption : Option[HeapNode], v : Int) : HeapNode = {
    nodeOption match {
      case Some(h) => {
        (h.leftSize <= h.rightSize, v > h.value) match {
          case (true, true) => {
            val newValue = v
            val newLeftChild = maxPushDown(h.leftChild, h.value)
            new HeapNode(newValue, Some(newLeftChild), h.rightChild, h.leftSize + 1, h.rightSize)
          }
          case (true, false) => {
            val newValue = h.value
            val newLeftChild = maxPushDown(h.leftChild, v)
            new HeapNode(newValue, Some(newLeftChild), h.rightChild, h.leftSize + 1, h.rightSize)
          }
          case (false, true) => {
            val newValue = v
            val newRightChild = maxPushDown(h.rightChild, h.value)
            new HeapNode(newValue, h.leftChild, Some(newRightChild), h.leftSize, h.rightSize + 1)
          }
          case (false, false) => {
            val newValue = h.value
            val newRightChild = maxPushDown(h.rightChild, v)
            new HeapNode(newValue, h.leftChild, Some(newRightChild), h.leftSize, h.rightSize + 1)
          }
        }
      }
      case None => {
        new HeapNode(v)
      }
    }
  }

  def minPullUp(h : HeapNode) : Option[HeapNode] = {
    (h.leftChild, h.rightChild) match {
      case (Some(l), Some(r)) => {
        if (l.value < r.value) {
          val newValue = l.value
          val newLeftChild = minPullUp(l)
          Some(new HeapNode(newValue, newLeftChild, h.rightChild, h.leftSize - 1, h.rightSize))
        }
        else {
          val newValue = r.value
          val newRightChild = minPullUp(r)
          Some(new HeapNode(newValue, h.leftChild, newRightChild, h.leftSize, h.rightSize - 1))
        }
      }
      case (Some(l), None) => {
        val newValue = l.value
        val newLeftChild = minPullUp(l)
        Some(new HeapNode(newValue, newLeftChild, h.rightChild, h.leftSize - 1, h.rightSize))
      }
      case (None, Some(r)) => {
        val newValue = r.value
        val newRightChild = minPullUp(r)
        Some(new HeapNode(newValue, h.leftChild, newRightChild, h.leftSize, h.rightSize - 1))
      }
      case (None, None) => {
        None
      }
    }
  }

  def minPushDown(nodeOption : Option[HeapNode], v : Int) : HeapNode = {
    nodeOption match {
      case Some(h) => {
        (h.leftSize <= h.rightSize, v < h.value) match {
          case (true, true) => {
            val newValue = v
            val newLeftChild = minPushDown(h.leftChild, h.value)
            new HeapNode(newValue, Some(newLeftChild), h.rightChild, h.leftSize + 1, h.rightSize)
          }
          case (true, false) => {
            val newValue = h.value
            val newLeftChild = minPushDown(h.leftChild, v)
            new HeapNode(newValue, Some(newLeftChild), h.rightChild, h.leftSize + 1, h.rightSize)
          }
          case (false, true) => {
            val newValue = v
            val newRightChild = minPushDown(h.rightChild, h.value)
            new HeapNode(newValue, h.leftChild, Some(newRightChild), h.leftSize, h.rightSize + 1)
          }
          case (false, false) => {
            val newValue = h.value
            val newRightChild = minPushDown(h.rightChild, v)
            new HeapNode(newValue, h.leftChild, Some(newRightChild), h.leftSize, h.rightSize + 1)
          }
        }
      }
      case None => {
        new HeapNode(v)
      }
    }
  }

  class Median(val median : Int, val maxHeap : Option[HeapNode], val minHeap : Option[HeapNode], val maxSize : Int, val minSize : Int)


  def incrementState(stateNumber : Int, newNumber : Int) : Int = {
    val previousState : Median = lookupArray(stateNumber - 1)
    val oldMax = previousState.maxHeap
    val oldMin = previousState.minHeap
    (previousState.minSize == previousState.maxSize, newNumber >= previousState.median) match {
      case (true, true) => {
        val newMin = minPushDown(oldMin, newNumber)
        val newMedian = new Median(previousState.median, oldMax, Some(newMin), previousState.maxSize, previousState.minSize + 1)
        lookupArray(stateNumber) = newMedian
        previousState.median
      }
      case (true, false) => {
        val newMaxTemp = maxPushDown(oldMax, newNumber)
        val newMedianVal = newMaxTemp.value
        val newMax = maxPullUp(newMaxTemp)
        val newMin = minPushDown(oldMin, previousState.median)
        val newMedian = new Median(newMedianVal, newMax, Some(newMin), previousState.maxSize, previousState.minSize + 1)
        lookupArray(stateNumber) = newMedian
        newMedianVal
      }
      case (false, true) => {
        val newMinTemp = minPushDown(oldMin, newNumber)
        val newMedianVal = newMinTemp.value
        val newMin = minPullUp(newMinTemp)
        val newMax = maxPushDown(oldMax, previousState.median)
        val newMedian = new Median(newMedianVal, Some(newMax), newMin, previousState.maxSize + 1, previousState.minSize)
        lookupArray(stateNumber) = newMedian
        newMedianVal
      }
      case (false, false) => {
        val newMax = maxPushDown(oldMax, newNumber)
        val newMedian = new Median(previousState.median, Some(newMax), oldMin, previousState.maxSize + 1, previousState.minSize)
        lookupArray(stateNumber) = newMedian
        previousState.median
      }
    }
  }

  def rollbackState(stateNumber : Int, rollbackNumber : Int) : Int = {
    val oldState = lookupArray(stateNumber + rollbackNumber)
    lookupArray(stateNumber) = oldState
    oldState.median
  }



  def run : Unit = {
    val t = readInt()

    var stateNumber : Int = 1
    val firstNumber = readInt()
    println(firstNumber)
    val initialMedian = new Median(firstNumber, None, None, 0, 0)
    lookupArray(1) = initialMedian

    for (i <- 0 until t - 1) {
      val n = readInt()
      stateNumber += 1
      if (n > 0) {
        val retVal = incrementState(stateNumber, n)
        println(retVal)
      }
      else {
        val retVal = rollbackState(stateNumber, n)
        println(retVal)
      }
    }
  }


  def testRun : Unit = {
    val r = new Random()
    val initialMedian = new Median(r.nextInt(1000000000), None, None, 0, 0)
    lookupArray(1) = initialMedian

    time {
      for (j <- 2 until 100001) {
        incrementState(j, r.nextInt(1000000000))
      }
    }


  }


  def main(args : Array[String]) : Unit = {
    run

  }

}
