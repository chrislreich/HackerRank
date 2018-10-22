package FunctionalProgramming

/**
  * Created by creich on 2/20/18.
  */
object BufStreamTest {




  def main(args : Array[String]) : Unit = {
    val src  = scala.io.Source.fromInputStream(System.in).bufferedReader()

    val line = src.readLine().split(" ")
    val n = line(0).toInt
    val q = line(1).toInt


    println(src)
  }

}
