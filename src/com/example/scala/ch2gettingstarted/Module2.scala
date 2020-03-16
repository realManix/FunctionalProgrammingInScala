package com.example.scala.ch2gettingstarted

object Module2 {
  /**
   * Specifying return type as it is recommended for public methods.
   */
  def abs(v: Int): Int = 
    if (v > 0) v
    else -v
  
  def factorial(n: Int): Int = {
      def go(n: Int, acc: Int): Int = 
        if (n <= 0) acc
        else go(n - 1, n * acc)
      return go(n, 1)
    }
  
  /**
   * Exercise 1
   */
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, a: Int, b: Int): Int = 
      if (i == n - 3) b + (a + b)
      else go(i + 1, b, a + b)
    if (n <= 0) 0
    else if (n == 1 || n == 2) 1
    else go(0, 0, 1)
  }
  
  /**
   * Exercise 2 
   */
  def isSorted[A](a: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(k: Int): Boolean = {
      if (k == a.length - 1) true
      else if (gt(a(k), a(k + 1))) false
      else go(k + 1)
    }
    go(0)
  }
  
  /**
   * Exercise 3
   */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    f(a, _)
  
  /**
   * Exercise 4
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    (a) => f(a, _)
  
  /**
   * Exercise 5
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = 
    (a, b) => f(a)(b)
  
  /**
   * Exercise 6
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    a => f(g(a))
  
  private def formatMsg(x: Int) = {
    s"The absoulte value of $x is ${abs(x)}." 
  }
  
  def main(args: Array[String]): Unit = {
    // Compile - "scalac Module2.scala"
    // Run - "scala Module2"
    println(formatMsg(-13))
    println(s"5! = ${factorial(5)}")
    
    // Ex. 1
    for (i <- 1 to 6)
      println(s"fib($i) = ${fibonacci(i)}")
      
    // Ex. 2
    val a1: Array[Int] = Array(1, 2, 3, 4, 5, 7, 10)
    val a2: Array[Int] = Array(1, 2, 5, 3, 6, 7, 8)
    val a3: Array[Float] = Array(1f, 2f, 3f, 100f, 4f)
    val a4: Array[Double] = Array(1.0 , 40.0, 45.0, 100.0, 1000.0, 1000.0)
    println("a1 sorted? " + isSorted(a1, (i1: Int, i2: Int) => i1 > i2))
    println("a2 sorted? " + isSorted(a2, (i1: Int, i2: Int) => i1 > i2))
    println("a3 sorted? " + isSorted(a3, (i1: Float, i2: Float) => i1 > i2))
    println("a4 sorted? " + isSorted(a4, (i1: Double, i2: Double) => i1 > i2))
  }
}