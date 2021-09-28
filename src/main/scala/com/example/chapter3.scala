package com.example

import scala.annotation.tailrec

import scala.Numeric

package datastructure {
  sealed trait List[+A] {
    def isEmpty: Boolean
    def tail: List[A]
    def setHead[B >: A](x: B): List[B] = Nil
    def drop(n: Int): List[A] = {
      @tailrec
      def iter(rest: List[A], n: Int): List[A] = {
        if n > 0 && !rest.isEmpty
        then iter(rest.tail, n-1)
        else rest
      }
      iter(this, n)
    }
    def dropWhile(f: A => Boolean): List[A] = {
      @tailrec
      def iter(rest: List[A]): List[A] = {
        rest match {
          case Nil => Nil
          case Cons(x, xs) =>
            if f(x) then iter(xs) else rest
        }
      }
      iter(this)
    }
    def init: List[A] = {
      this match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, xs.init)
      }
    }
    def foldRight[B](defaultValue: B)(combine: (A, B) => B): B =
      this match {
        case Nil => defaultValue
        case Cons(x, xs) =>
          combine(x, xs.foldRight(defaultValue)(combine))
      }
    def sum[B >: A](implicit n: Numeric[B]): B =
      foldRight(n.zero)(n.plus)
    def product[B >: A](implicit n: Numeric[B]): B =
      foldRight(n.one)(n.times)
  }
  case object Nil extends List[Nothing] {
    val isEmpty = true
    override def tail: List[Nothing] = Nil
  }
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    val isEmpty = false
    override def setHead[B >: A](x: B): List[B] = Cons(x, tail)
  }

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](xs: A*): List[A] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))
  }
}
