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
    def foldLeft[B](defaultValue: B)(combine: (B, A) => B): B = {
      @tailrec
      def iter(rest: List[A], result: B): B = {
        rest match {
          case Nil => result
          case Cons(x, xs) => iter(xs, combine(result, x))
        }
      }
      iter(this, defaultValue)
    }
    def foldRight[B](defaultValue: B)(combine: (A, B) => B): B =
      this match {
        case Nil => defaultValue
        case Cons(x, xs) =>
          combine(x, xs.foldRight(defaultValue)(combine))
      }
    def sum[B >: A](implicit n: Numeric[B]): B = {
      @tailrec
      def iter(rest: List[A], result: B): B = {
        rest match {
          case Nil => result
          case Cons(x, xs) => iter(xs, n.plus(x, result))
        }
      }
      iter(this, n.zero)
    }
    def product[B >: A](implicit n: Numeric[B]): B = {
      @tailrec
      def iter(rest: List[A], result: B): B = {
        rest match {
          case Nil => result
          case Cons(x, xs) => iter(xs, n.times(x, result))
        }
      }
      iter(this, n.one)
    }
    def length: Int = {
      @tailrec
      def iter(rest: List[A], result: Int): Int = {
        rest match {
          case Nil => result
          case Cons(_, xs) => iter(xs, result+1)
        }
      }
      iter(this, 0)
    }
    def reverse: List[A] = {
      @tailrec
      def iter(rest: List[A], result: List[A]): List[A] = {
        rest match {
          case Nil => result
          case Cons(x, xs) => iter(xs, Cons(x, result))
        }
      }
      iter(this, Nil)
    }
  }
  case object Nil extends List[Nothing] {
    val isEmpty = true
    override def tail: List[Nothing] = Nil
  }
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    val isEmpty = false
    override def setHead[B >: A](x: B): List[B] = Cons(x, tail)
  }

  trait ListUtilisesFoldRight[A] { self: List[A] =>
    override def sum[B >: A](implicit n: Numeric[B]): B =
      foldRight(n.zero)(n.plus)
    override def product[B >: A](implicit n: Numeric[B]): B =
      foldRight(n.one)(n.times)
    override def length: Int =
      foldRight(0) { (_: A, count: Int) => count + 1 }
    override def reverse: List[A] = {
      def identity(x: List[A]): List[A] = x
      val nil: List[A] = Nil
      foldRight(identity){
        (x: A, acc: (List[A]) => List[A]) => {
          (next: List[A]) =>
          acc(Cons(x, next))
        }
      }(nil)
    }
  }
  trait ListUtilisesFoldLeft[A] { self: List[A] =>
    override def sum[B >: A](implicit n: Numeric[B]): B =
      foldLeft(n.zero)(n.plus)
    override def product[B >: A](implicit n: Numeric[B]): B =
      foldLeft(n.one)(n.times)
    override def length: Int =
      foldLeft(0) { (count: Int, _: A) => count + 1 }
    override def reverse: List[A] =
      foldLeft(Nil: List[A]) {
        (acc: List[A], x: A) => Cons(x, acc)
      }
  }
  trait ListUsingStrangeFoldRight[A] { self: List[A] =>
    override def foldRight[B](default: B)(combine: (A, B) => B): B = {
      def identity(x: B): B = x
      foldLeft(identity) {
        (acc: B => B, x: A) => {
          (next: B) =>
          acc(combine(x, next))
        }
      }(default)
    }
  }
  trait ListUsingStrangeFoldLeft[A] { self: List[A] =>
    override def foldLeft[B](default: B)(combine: (B, A) => B): B = {
      def identity(x: B): B = x
      foldRight(identity) {
        (x: A, acc: B => B) => {
          (next: B) =>
          acc(combine(next, x))
        }
      }(default)
    }
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
