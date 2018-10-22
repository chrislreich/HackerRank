package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object SequenceEquation {

  def permutationEquation(p: Array[Int]): Array[Int] = {

    val reverseIndexList = (0 :: p.toList).zipWithIndex.tail
    val reverseIndexMap = reverseIndexList.toMap

    val result = for (i <- 1 until p.length + 1) yield reverseIndexMap.getOrElse(reverseIndexMap.getOrElse(i, -1), -1)

    result.toArray
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val n = stdin.readLine.trim.toInt

    val p = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = permutationEquation(p)

    println(result.mkString)

  }

}
