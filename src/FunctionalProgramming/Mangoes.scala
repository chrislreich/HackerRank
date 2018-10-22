package FunctionalProgramming

/**
  * Created by creich on 4/5/18.
  */
object Mangoes {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  def findNSum(n : Int, friends : List[(Long, Long)]) : Long = {
    val friendsVal = friends.map(x => (x._1, x._2, (x._1 + (x._2 * (n - 1)))))
    val sorted = friendsVal.sortBy(x => x._3)
    val nLowest = sorted.take(n)
    val sum = nLowest.foldLeft(0.toLong)((b, x) => b + x._3)
    sum
  }

  def findNumFriends(n : Int, m : Long, friends : List[(Long, Long)]) : Int = {
    def binarySearch(curFloor : Int, curCeiling : Int) : Int = {
      if (curFloor + 1 == curCeiling) {
        val retLong = findNSum(curCeiling, friends)
        if (retLong <= m) {
          curCeiling
        }
        else{
          curFloor
        }
      }
      else {
        val mid = (curFloor + curCeiling) / 2
        val retLong = findNSum(mid, friends)
        if (retLong <= m) {
          binarySearch(mid, curCeiling)
        }
        else {
          binarySearch(curFloor, mid)
        }
      }
    }
    binarySearch(0, n)
  }

  def run : Unit = {
    val first = readLine().split(" ")
    val n = first(0).toInt
    val m = first(1).toLong

    val appetites = readLine().split(" ").map(_ toLong)
    val happiness = readLine().split(" ").map(_ toLong)

    val zipped = (appetites zip happiness).toList

    val answer = findNumFriends(n, m, zipped)
    println(answer)
  }

  def test : Unit = {
    val first = readLine().split(" ")
    val n = first(0).toInt
    val m = first(1).toLong

    val appetites = readLine().split(" ").map(_ toLong)
    val happiness = readLine().split(" ").map(_ toLong)

    val zipped = (appetites zip happiness).toList

    val trial = findNSum(22328, zipped)

    val answer = time{findNumFriends(50000, m, zipped)}
    println(answer)
  }



  def main(args : Array[String]) : Unit = {
    run
  }

}
