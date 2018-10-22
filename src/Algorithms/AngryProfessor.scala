package Algorithms

/**
  * Created by creich on 8/10/18.
  */
object AngryProfessor {

  def angryProfessor(k: Int, a: Array[Int]): String = {

    val map = a.groupBy(x => if (x <= 0) "OnTime" else "Late").mapValues(_.length)

    map.get("OnTime") match {
      case Some(v) => {
        if (v >= k) {
          "NO"
        }
        else {
          "YES"
        }
      }
      case None => {
        "YES"
      }
    }


  }

}
