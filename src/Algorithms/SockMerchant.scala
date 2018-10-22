package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object SockMerchant {

  def sockMerchant(n: Int, ar: Array[Int]): Int = {

    ar.groupBy(x => x).mapValues(_.length).mapValues(_ / 2).foldLeft(0)((b, t) => b + t._2)

  }

}
