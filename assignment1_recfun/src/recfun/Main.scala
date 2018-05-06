package recfun

/**
  * Created by Petro Karabyn
  * on 06-May-18
  */
object Main {
  def main(args: Array[String]) {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balancer")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance(" ( () () )".toList))
    println(balance("((()))(".toList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    factorial(r) / (factorial(c) * factorial(r - c))
  }

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if(n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def diff(chars: List[Char], delta: Int): Int = {
      if(chars.isEmpty) return delta
      val char = chars.head
      if(char == '(') {
        diff(chars.tail, delta + 1)
      } else if(char == ')') {
        if(delta == 0) delta - 1
        else diff(chars.tail, delta - 1)
      } else {
        diff(chars.tail, delta)
      }
    }

    diff(chars, 0) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
