object Exercise2 {

  def sum(f:  Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }

  sum(x => x * x, 3, 5)

  // functions returning other functions
  // Currying

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1 , b)
    sumF
  }
  sum(x => x * x)(3, 5)

  def sumShort(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumShort(f)(a + 1, b)
  sumShort(x => x * x)(3, 5)

  def sumInts = sum(x => x)
  sumInts(0, 5)

  def sumCubes = sum(x => x * x * x)

  def product(f: Int => Int): (Int, Int) => Int = {
    def productF(a: Int, b: Int): Int =
      if (a > b) 1
      else f(a) * product(f)(a + 1, b)

    productF
  }
  product(x => x * x)(3, 4)

  def productShort(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * productShort(f)(a + 1, b)
  }
  productShort(x => x * x)(3, 4)

  def fact(n: Int) = product(x => x)(1, n)
  fact(5)

  // generalized function
  // a version of map reduce
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def productMP(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
  productMP(x => x * x)(3, 4)
}