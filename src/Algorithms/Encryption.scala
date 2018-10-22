package Algorithms

/**
  * Created by creich on 8/16/18.
  */

import scala.collection.immutable.SortedMap
object Encryption {

  def encryption(s: String): String = {
    val noSpaces = s.filter(_ != ' ')
    val filteredLength = noSpaces.length

    val root = scala.math.sqrt(filteredLength)

    val floor = scala.math.floor(root)
    val ceiling = scala.math.ceil(root)

    val numRows = if (floor * ceiling < filteredLength) floor + 1 else floor

    val zippedNoSpaces = noSpaces zipWithIndex

    val groupedByModulo: Map[Int, String] = zippedNoSpaces.groupBy(x => (x._2 % ceiling).toInt).mapValues(y => y.map(z => z._1)).mapValues(a => a.mkString)

    val sortedByModulo : SortedMap[Int, String] = SortedMap[Int, String]() ++ groupedByModulo

    sortedByModulo.values.mkString(" ")

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn



    val s = stdin.readLine

    val result = encryption(s)

    println(result)
  }

}
