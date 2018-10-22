package FunctionalProgramming

/**
  * Created by creich on 2/20/18.
  */
object TreeManager {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }
  class Node(var value : Int, var parent : Option[Node] = None, var leftSibling : Option[Node] = None, var rightSibling : Option[Node] = None, var children : Option[Node] = None)


  def deleteNode(n : Node) : Node = {
    (n.rightSibling, n.leftSibling) match {
      case (Some(r), Some(l)) => {
        r.leftSibling = Some(l)
        l.rightSibling = Some(r)
        n.parent match {case Some(x) => x}
      }
      case (Some(r), None) => {
        r.leftSibling = None
        n.parent match {
          case Some(x) => {
            x.children = Some(r)
            x
          }
        }
      }
      case (None, Some(l)) => {
        l.rightSibling = None
        n.parent match {case Some(x) => x}
      }
      case (None, None) => {
        n.parent match {
          case Some(x) => {
            x.children = None
            x
          }
        }
      }
    }
  }

  def getChild(n : Option[Node], num : Int) : Node = {
    if (num == 1) {
      n match {case Some(x) => x}
    }
    else {
      val current = n match{case Some(x) => x}
      getChild(current.rightSibling, num - 1)
    }
  }
  def insertNewNode(n : Node, direction : String, newValue : Int) : Node = {
    val newNode = new Node(newValue)
    direction match {
      case "child" => {
        newNode.parent = Some(n)
        n.children match {
          case Some(x) => {
            newNode.rightSibling = Some(x)
            x.leftSibling = Some(newNode)
            n.children = Some(newNode)
            n
          }
          case None => {
            n.children = Some(newNode)
            n
          }
        }
      }
      case "left" => {
        newNode.parent = n.parent
        n.leftSibling match {
          case Some(x) => {
            newNode.leftSibling = Some(x)
            newNode.rightSibling = Some(n)
            x.rightSibling = Some(newNode)
            n.leftSibling = Some(newNode)
            n
          }
          case None => {
            newNode.rightSibling = Some(n)
            n.leftSibling = Some(newNode)
            n.parent match {
              case Some(x2) => {
                x2.children = Some(newNode)
                n
              }
            }
          }
        }
      }
      case "right" => {
        newNode.parent = n.parent
        n.rightSibling match {
          case Some(x) => {
            newNode.rightSibling = Some(x)
            newNode.leftSibling = Some(n)
            x.leftSibling = Some(newNode)
            n.rightSibling = Some(newNode)
            n
          }
          case None => {
            newNode.leftSibling = Some(n)
            n.rightSibling = Some(newNode)
            n
          }
        }
      }
    }
  }

  def dispatch(n : Node, q : String) : Node = {
    val strArr = q.split(" ")
    strArr.length match {
      case 1 => {
        strArr(0) match {
          case "print" => {
            println(n.value)
            n
          }
          case "delete" => deleteNode(n)
        }
      }
      case 2 => {
        strArr(0) match {
          case "change" => {
            n.value = strArr(1).toInt
            n
          }
          case "visit" => strArr(1) match {
            case "left" => n.leftSibling match {case Some(x) => x}
            case "right" => n.rightSibling match {case Some(x) => x}
            case "parent" => n.parent match {case Some(x) => x}
          }
        }
      }
      case 3 => {
        strArr(0) match {
          case "visit" => {
            val childNumber = strArr(2).toInt
            getChild(n.children, childNumber)
          }
          case "insert" => {
            insertNewNode(n, strArr(1), strArr(2).toInt)
          }
        }
      }
    }
  }

  def main(args : Array[String]) : Unit = {
    val q = readInt()

    var currentNode = new Node(0)

    for (i <- 0 until q) {
      val nextLine = readLine()
      currentNode = dispatch(currentNode, nextLine)
    }

  }

}
