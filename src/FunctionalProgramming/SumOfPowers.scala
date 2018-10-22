package FunctionalProgramming

/**
  * Created by creich on 1/19/18.
  */
object SumOfPowers {
  def numberOfWays(X:Int,N:Int):Int = {
    val limit = scala.math.pow(X, (1.0/N)).floor.toInt

    val powMap = new scala.collection.mutable.HashMap[Int, Int]()

    for (i <- 1 until limit + 1)
      powMap += i -> scala.math.pow(i, N).toInt


    def findSums(curSum : Int, intList : List[Int], curInt : Int) : Set[Option[List[Int]]] = {
      if (curSum == X)
        Set(Some(intList))
      else {
        if (curSum > X || curInt < 1)
          Set(None)
        else {
          var retSet = Set[Option[List[Int]]]()
          for (k <- curInt - 1 until -1 by -1) {
            retSet = retSet ++ findSums(curSum + powMap.getOrElse(curInt, 0), curInt :: intList, k)
          }
          retSet
        }
      }

    }

    var result = Set[Option[List[Int]]]()
    for (j <- limit until 0 by -1)
      result = result ++ findSums(0,Nil, j)

    result.filter(_ nonEmpty).size
  }


  def main(args: Array[String]) {
    println(numberOfWays(readInt(),readInt()))
  }



}
