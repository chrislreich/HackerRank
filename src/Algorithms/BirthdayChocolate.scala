package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object BirthdayChocolate {

  def birthday(s: Array[Int], d: Int, m: Int): Int = {
    s.toList.sliding(m, 1).filter(_.sum == d).size
  }

  def main(args: Array[String]) : Unit = {

  }

}
