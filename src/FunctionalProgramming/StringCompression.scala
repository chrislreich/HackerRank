package FunctionalProgramming


import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
/**
  * Created by creich on 1/12/18.
  */
object StringCompression {


  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + "ns")
    result
  }

  var inputString = ""

  def compressInputString(str : String) : List[(Char, Int)] = {
    @tailrec
    def helper(returnList : List[(Char, Int)], curString : String) : List[(Char, Int)] = {
      if (curString.isEmpty) returnList
      else {
        val curChar = curString.head
        val spans = curString.span(_ == curChar)
        val curSpan = spans._1
        val restSpan = spans._2


        helper((curChar, spans._1.length) :: returnList, spans._2)
      }
    }



    helper(List.empty, str).reverse

  }


  def printList(input : List[(Char, Int)]) : Unit = {
    input match {
      case x :: xs => {
        if (x._2 == 1) {
          print(x._1)
          printList(xs)
        }
        else {
          print(x._1 + x._2.toString)
          printList(xs)
        }
      }
      case _ => print("")
    }
  }






  def parCompressString(str : String) : List[(Char, Int)] = {


    def mergeLists(a : List[(Char, Int)], b : List[(Char, Int)]) : List[(Char, Int)] = {
      val aLast = a.last
      val bFirst = b.head
      if (aLast._1 == bFirst._1) {
        val newTuple = List[(Char, Int)]((aLast._1, aLast._2 + bFirst._2))
        a.dropRight(1) ++ newTuple ++ b.tail
      }
      else
        a ++ b
    }

    def helper(inputList : List[Char]) : List[(Char, Int)] = {
      val listLength = inputList.length
      if (listLength == 1)
        List[(Char, Int)]((inputList.head, 1))
      else {
        val halfListLength = listLength / 2
        val splitLists = inputList.splitAt(halfListLength)
        val firstHalfCompressed = Future { helper(splitLists._1)}
        val secondHalfCompressed = Future{ helper(splitLists._2)}

        val mergedLists: Future[List[(Char, Int)]] =
          for {
            first <- firstHalfCompressed
            second <- secondHalfCompressed
          }
            yield mergeLists(first, second)

        Await.result(mergedLists, 10 second)
      }
    }

    helper(str.toList)

  }

  def printOutput(input : List[(Char, Int)]) : Unit = {
    input match {
      case x :: xs => {
        if (x._2 == 1) {
          print(x._1)
          printOutput(xs)
        }
        else {
          print(x._1.toString + x._2.toString)
          printOutput(xs)
        }
      }
      case _ => print("")
    }
  }





  def compress(xs: String): List[(Char, Int)] = {
    @tailrec
    def go(acc: List[(Char, Int)], ys: String): List[(Char, Int)] = {
      if(ys.isEmpty){
        acc
      } else {
        val y: Char = ys.head
        val spans: (String, String) = ys.span(_ == y)
        go((y, spans._1.length) :: acc, spans._2)
      }
    }
    go(Nil, xs).reverse
  }

  def format_count_tuple[T](tups: List[(T, Int)]): String = {
    tups.map(x => {
      if(x._2 == 1){
        s"${x._1}"
      } else {
        s"${x._1}${x._2}"
      }
    }).mkString("")
  }





  def main(args : Array[String]) : Unit = {
    val sc = new java.util.Scanner(System.in)
    inputString = sc.nextLine().trim()

    //inputString = "aaabaaaaccaaaaba"

    printList(compressInputString(inputString))


    //printOutput(parCompressString(inputString))
  }

}
