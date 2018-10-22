package Algorithms

/**
  * Created by creich on 6/5/18.
  */
import java.io.PrintWriter

object GradeRounding {

  /*
   * Complete the gradingStudents function below.
   */
  def gradingStudents(grades: Array[Int]): Array[Int] = {
    grades.map(gradingHelper)

  }

  def gradingHelper(x : Int) : Int = {
    if (x < 38) {
      x
    }
    else {
      val base = x / 5
      val mod = x % 5
      if (mod > 2) {
        (base + 1) * 5
      }
      else {
        x
      }
    }
  }

  def main(args: Array[String]) {
    val scan = scala.io.StdIn

    //val fw = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = scan.readLine.trim.toInt

    val grades = Array.ofDim[Int](n)

    for (gradesItr <- 0 until n) {
      val gradesItem = scan.readLine.trim.toInt
      grades(gradesItr) = gradesItem}

    val result = gradingStudents(grades)

    println(result.mkString("\n"))

    //fw.close()
  }
}

