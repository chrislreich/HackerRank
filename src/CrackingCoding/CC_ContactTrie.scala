package CrackingCoding

/**
  * Created by creich on 5/22/18.
  */


object CC_ContactTrie {

  class Node(val sub_words : Int = 0, val map : Map[Char, Node] = Map[Char, Node]()) {
    def mkString : String = {
      "Sub_Words: " + sub_words + "\n Chars: " + map.mkString
    }
  }


  def addContact(node : Node, str : String) : Node = {
    if (str isEmpty) {
      val retNode = new Node(1)
      //println(retNode.mkString)
      retNode
    }
    else {
      node.map get str.head match {
        case None => {
          val newNode = new Node()
          val subNode = addContact(newNode, str.tail)
          val retNode = new Node(node.sub_words + 1, node.map + (str.head -> subNode))
          //println(retNode.mkString)
          retNode
        }
        case Some(n) => {
          val subNode = addContact(n, str.tail)
          val retNode = new Node(node.sub_words + 1, node.map + (str.head -> subNode))
          //println(retNode.mkString)
          retNode
        }
      }
    }
  }

  def findPartial(node : Node, str : String) : Int = {
    if (str isEmpty) {
      //println(node.mkString)
      node.sub_words
    }
    else {
      node.map get str.head match {
        case None => 0
        case Some(n) => {
         // println(n.mkString)
          findPartial(n, str.tail)
        }
      }
    }
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    var root = new Node()

    for (nItr <- 1 to n) {
      val opContact = stdin.readLine.split(" ")

      val op = opContact(0)

      val contact = opContact(1)
      if (op == "add") {
        root = addContact(root, contact)
      }
      else {
        println(findPartial(root, contact))
      }
    }
  }
}