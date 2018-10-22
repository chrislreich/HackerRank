package FunctionalProgramming

/**
  * Created by creich on 1/17/18.
  */
import scala.io.StdIn._
import scala.util.Random
object StringReduction {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0) / 10e8) + " s")
    result
  }

  def generateRandomString(len : Int) : Stream[Char] = {
    val generator = Random.alphanumeric

    generator take len
  }

  def reduceChars(curList : List[Char], curChar : Char) : List[Char] = {
    if (curList.exists(_ == curChar))
      curList
    else
      curChar :: curList
  }


  def main(args : Array[String]) : Unit = {
    val inputString = readLine()



    val result = inputString.foldLeft(List[Char]())(reduceChars).reverse
    println(result.mkString)

  }

}
