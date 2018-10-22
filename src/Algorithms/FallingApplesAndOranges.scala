package Algorithms

/**
  * Created by creich on 6/5/18.
  */
object FallingApplesAndOranges {




  def countApplesAndOranges(s: Int, t: Int, a: Int, b: Int, apples: Array[Int], oranges: Array[Int]) : Unit = {
    def countApples(appleList : List[Int], count : Int) : Int = {
      appleList match {
        case x :: xs => {
          val position = a + x
          if (position >= s && position <= t) {
            countApples(xs, count + 1)
          }
          else {
            countApples(xs, count)
          }
        }
        case _ => {
          count
        }
      }
    }

    def countOranges(orangeList : List[Int], count : Int) : Int = {
      orangeList match {
        case x :: xs => {
          val position = b + x
          if (position >= s && position <= t) {
            countOranges(xs, count + 1)
          }
          else {
            countOranges(xs, count)
          }
        }
        case _ => {
          count
        }
      }
    }

    val appleCount = countApples(apples.toList, 0)
    val orangeCount = countOranges(oranges.toList, 0)

    println(appleCount)
    println(orangeCount)

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val st = stdin.readLine.split(" ")

    val s = st(0).trim.toInt

    val t = st(1).trim.toInt

    val ab = stdin.readLine.split(" ")

    val a = ab(0).trim.toInt

    val b = ab(1).trim.toInt

    val mn = stdin.readLine.split(" ")

    val m = mn(0).trim.toInt

    val n = mn(1).trim.toInt

    val apples = stdin.readLine.split(" ").map(_.trim.toInt)

    val oranges = stdin.readLine.split(" ").map(_.trim.toInt)
    countApplesAndOranges(s, t, a, b, apples, oranges)
  }

}
