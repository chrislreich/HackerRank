package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object MigratoryBirds {

  def migratoryBirds(arr: Array[Int]): Int = {
    def helper(list : List[Int], map : Map[Int, Int]) : Map[Int, Int] = {
      list match {
        case x :: xs => {
          map.get(x) match {
            case Some(v) => {
              helper(xs, map + (x -> (v + 1)))
            }
            case None => {
              helper(xs, map + (x -> 1))
            }
          }
        }
        case _ => {
          map
        }
      }
    }

    val resultMap = helper(arr.toList, Map[Int, Int]())

    resultMap.maxBy(x => (x._2, -x._1))._1


  }



}
