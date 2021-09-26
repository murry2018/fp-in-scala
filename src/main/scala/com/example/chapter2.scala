package com.example

object Chapter2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def iter(i: Int, c: Int, p: Int): Int = {
      if i == n then
        c
      else
        iter(i+1, c+p, c)
    }
    if n < 0 then  throw new ArithmeticException()
    else if n == 0 then 0
    else iter(1, 1, 0)
  }

  def isSorted[A](xs: Array[A],
    ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def iter(i: Int): Boolean = {
      if i >= xs.size then true
      else if !ordered(xs(i-1), xs(i)) then false
      else iter(i+1)
    }
    iter(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
