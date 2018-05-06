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

    println("\nBalancer")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance(" ( () () )".toList))
    println(balance("((()))(".toList))

    println("\nCounting Change")
    println(countChange(4,List(1,2))) // 3
    println(countChange(300,List(5,10,20,50,100,200,500))) // 1022
    println(countChange(301,List(5,10,20,50,100,200,500))) // 0
    println(countChange(300,List(500,5,50,100,20,200,10))) // 1022
    println(countChange(1,List())) // 0
    println(countChange(0,List(5,10,20,50,100))) // 0
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
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeAux(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(coins.isEmpty) 0
      else if(money < 0 && coins.nonEmpty) 0
      else countChangeAux(money - coins.head, coins) + countChangeAux(money, coins.tail)
    }

    if(coins.isEmpty || money <= 0) 0
    else countChangeAux(money,coins)
  }

  def countChangeNonRecursive(money: Int, coins: List[Int]): Int = {
    val c_array = Array.fill[Int](money + 1)(0)
    c_array.update(0, 1)

    for(k <- coins) {
      for(i <- 0 to money - k) {
        val sum = c_array(i) + c_array(i + k)
        c_array.update(i + k, sum)
      }
    }
    c_array(money)
  }
}
