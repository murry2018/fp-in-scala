package com.example

import scala.annotation.tailrec

import scala.Numeric
import scala.math.Numeric.Implicits.infixNumericOps

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
  }
  case object Nil extends List[Nothing] {
    val isEmpty = true
    override def tail: List[Nothing] = Nil
  }
  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    val isEmpty = false
    override def setHead[B >: A](x: B): List[B] = Cons(x, tail)
  }
  object Implicits {
    implicit class ListUtilisesRecursion[A](self: List[A]) {
      def foldRight[B](default: B)(combine: (A, B) => B): B =
        self match {
          case Nil => default
          case Cons(x, xs) =>
            combine(x,
              ListUtilisesRecursion(xs)
                .foldRight(default)(combine))
        }
      def foldLeft[B](default: B)(combine: (B, A) => B): B = {
        @tailrec
        def iter(rest: List[A], result: B): B = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, combine(result, x))
          }
        }
        iter(self, default)
      }
      def sum[B >: A : Numeric]: B = {
        @tailrec
        def iter(rest: List[A], result: B): B = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, result + x)
          }
        }
        iter(self, implicitly[Numeric[B]].zero)
      }
      def product[B >: A : Numeric]: B = {
        @tailrec
        def iter(rest: List[A], result: B): B = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, result * x)
          }
        }
        iter(self, implicitly[Numeric[B]].one)
      }
      def length: Int = {
        @tailrec
        def iter(rest: List[A], result: Int): Int = {
          rest match {
            case Nil => result
            case Cons(_, xs) => iter(xs, result+1)
          }
        }
        iter(self, 0)
      }
      def reverse: List[A] = {
        @tailrec
        def iter(rest: List[A], result: List[A]): List[A] = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, Cons(x, result))
          }
        }
        iter(self, Nil)
      }
    }

    implicit class ListUtilisesFoldLeft[A](self: List[A]) {
      def foldLeft[B](default: B)(combine: (B, A) => B): B = {
        @tailrec
        def iter(rest: List[A], result: B): B = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, combine(result, x))
          }
        }
        iter(self, default)
      }
      def foldRight[B](default: B)(combine: (A, B) => B): B =
        ListUtilisesFoldLeft(ListUtilisesFoldLeft(self).reverse)
          .foldLeft(default)((acc, x) => combine(x, acc))
      def foldRightUnsafe[B](default: B)(combine: (A, B) => B): B = {
        def identity(x: B): B = x
        foldLeft(identity) {
          (acc: B => B, x: A) => {
            (next: B) =>
            acc(combine(x, next))
          }
        }(default)
      }
      def sum[B >: A](implicit n: Numeric[B]): B =
        foldLeft(n.zero)(n.plus)
      def product[B >: A](implicit n: Numeric[B]): B =
        foldLeft(n.one)(n.times)
      def length: Int =
        foldLeft(0) { (count: Int, _: A) => count + 1 }
      def reverse: List[A] =
        foldLeft(Nil: List[A]) {
          (acc: List[A], x: A) => Cons(x, acc)
        }
    }

    implicit class ListUtilisesFoldRight[A](self: List[A]) {
      def foldRight[B](default: B)(combine: (A, B) => B): B =
        self match {
          case Nil => default
          case Cons(x, xs) =>
            combine(x,
              ListUtilisesFoldRight(xs)
                .foldRight(default)(combine))
        }
      def sum[B >: A](implicit n: Numeric[B]): B =
        foldRight(n.zero)(n.plus)
      def product[B >: A](implicit n: Numeric[B]): B =
        foldRight(n.one)(n.times)
      def length: Int =
        foldRight(0) { (_: A, count: Int) => count + 1 }
      def reverse: List[A] = {
        def identity(x: List[A]): List[A] = x
        val nil: List[A] = Nil
        foldRight(identity){
          (x: A, acc: (List[A]) => List[A]) => {
            (next: List[A]) =>
            acc(Cons(x, next))
          }
        }(nil)
      }
      def foldLeft[B](default: B)(combine: (B, A) => B): B = {
        def identity(x: B): B = x
        foldRight(identity) {
          (x: A, acc: B => B) => {
            (next: B) =>
            acc(combine(next, x))
          }
        }(default)
      }
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
