package Algorithms

/**
  * Created by creich on 8/8/18.
  */
object DayOfTheProgrammer {


  def solve(year: Int): String = {
    if (year == 1918) {
      "26.09.1918"
    }
    else {
      if (year < 1918) {
        if (year % 4 == 0) {
          "12.09." + year.toString()
        }
        else {
          "13.09." + year.toString()
        }
      }
      else {
        if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) {
          "12.09." + year.toString()
        }
        else {
          "13.09." + year.toString()
        }
      }
    }
  }

}
