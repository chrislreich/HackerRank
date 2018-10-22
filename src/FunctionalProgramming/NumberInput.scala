package FunctionalProgramming

/**
  * Created by creich on 2/26/18.
  */
object NumberInput {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * 10e-10 + " sec")
    result
  }

  case class CacheState(left : Int, right : Int, sum : Int)




  def nextCacheStates(stateList : List[CacheState], n : Int) : List[CacheState] = {


    def computeDistance(i : Int) : Int = {
      (i == 0, n == 0) match {
        case (true, true) => {
          0
        }
        case (true, false) => {
          10 - n
        }
        case (false, true) => {
          10 - i
        }
        case (false, false) => {
          scala.math.abs(i - n)
        }
      }
    }


    def nextStates(c : CacheState) : List[CacheState] = {
      c match {
        case CacheState(l, -1, s) => {
          if (l == n) {
            List(CacheState(l, -1, s + 1))
          }
          else {
            List(CacheState(n, -1, s + computeDistance(l) + 1), CacheState(l, n, s + 1))
          }
        }
        case CacheState(l, r, s) => {
          if (l == n || r == n) {
            List(CacheState(l, r, s + 1))
          }
          else {
            val newR = if(r == 0) 10 else r
            val newL = if(l == 0) 10 else l
            val newN = if(n == 0) 10 else n
            if (newL > newR) {
              (newL > newN, newR > newN) match {
                case (true, true) => {
                  List(CacheState(l, n, s + 1 + computeDistance(r)))
                }
                case (false, false) => {
                  List(CacheState(n, r, s + 1 + computeDistance(l)))
                }
                case _ => {
                  List(CacheState(n, r, s + 1 + computeDistance(l)), CacheState(l, n, s + 1 + computeDistance(r)))
                }
              }
            }
            else {
              (newL > newN, newR > newN) match {
                case (true, true) => {
                  List(CacheState(n, r, s + 1 + computeDistance(l)))
                }
                case (false, false) => {
                  List(CacheState(l, n, s + 1 + computeDistance(r)))
                }
                case _ => {
                  List(CacheState(n, r, s + 1 + computeDistance(l)), CacheState(l, n, s + 1 + computeDistance(r)))
                }
              }
            }
          }
        }
      }
    }




    val grouped = stateList.flatMap(nextStates).groupBy(x => if (x.left > x.right) (x.left, x.right) else (x.right, x.left))
    val mapped = grouped.map(y => CacheState(y._1._1, y._1._2, y._2.minBy(j => j.sum).sum)).toList



    mapped



  }




  def processInput(list : List[Int]) : Int = {


    def go(currentCacheStates : List[CacheState], currentList : List[Int]) : Int = {
      currentList match {
        case x :: xs => {
          val nextStates = nextCacheStates(currentCacheStates, x)
          go(nextStates, xs)
        }
        case _ => currentCacheStates.minBy(x => x.sum).sum
      }
    }


    go(List(CacheState(list.head, -1, 1)), list.tail)
  }

  def test1 : Unit = {
    val r = scala.util.Random
    val input = (for (i <- 0 until 1000000) yield r.nextInt(9)).toList
    println(time{processInput(input)})

  }



  def run1 : Unit = {
    val n = readInt()
    val inputList = readLine().split(" ").toList.map(_.toInt)
    println(processInput(inputList))
  }

  def main(args : Array[String]) : Unit = {
    run1

  }

}
