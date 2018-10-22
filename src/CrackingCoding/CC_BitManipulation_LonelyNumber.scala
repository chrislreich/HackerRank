package CrackingCoding

/**
  * Created by creich on 5/31/18.
  */
object CC_BitManipulation_LonelyNumber {

  def run = {
    val n = readInt()
    val nums = readLine().split(" ").map(_.trim.toInt)

    val lonelyNum = nums.reduce((x, y) => x ^ y)
    println(lonelyNum)
  }

  def main(args : Array[String]) : Unit = {
    run

  }

}
