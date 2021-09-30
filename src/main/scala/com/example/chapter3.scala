package com.example

import scala.annotation.tailrec

import scala.Numeric
import scala.math.Numeric.Implicits.infixNumericOps
import com.example.datastructure.Implicits.ListImplicit

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
    implicit class ListCommonUtility[A](self: List[A])
      (implicit util: List[A] => ListImplicit[A]) {
      def map[B](f: A => B): List[B] = {
        @tailrec
        def iter(rest: List[A], result: List[B]): List[B] = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, Cons(f(x), result))
          }
        }
        iter(util(self).reverse, Nil)
      }
      def foreachAddOne(implicit num: Numeric[A]): List[A] =
        map(_+ num.one)
      def foreachToString: List[String] =
        map(_.toString)
      def filter(predicate: A => Boolean): List[A] = {
        @tailrec
        def iter(rest: List[A], result: List[A]): List[A] = {
          rest match {
            case Nil => result
            case Cons(x, xs) if predicate(x) =>
              iter(xs, Cons(x, result))
            case Cons(_, xs) => iter(xs, result)
          }
        }
        iter(util(self).reverse, Nil)
      }
    }

    sealed trait ListImplicit[A] {
      def foldRight[B](default: B)(combine: (A, B) => B): B
      def foldLeft[B](default: B)(combine: (B, A) => B): B
      def sum(implicit n: Numeric[A]): A
      def product(implicit n: Numeric[A]): A
      def length: Int
      def reverse: List[A]
      def append(lists: List[A]*): List[A]
    }
    implicit class ListUtilisesRecursion[A](self: List[A]) extends ListImplicit[A]{
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
      def sum(implicit n: Numeric[A]): A = {
        @tailrec
        def iter(rest: List[A], result: A): A = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, result + x)
          }
        }
        iter(self, n.zero)
      }
      def product(implicit n: Numeric[A]): A = {
        @tailrec
        def iter(rest: List[A], result: A): A = {
          rest match {
            case Nil => result
            case Cons(x, xs) => iter(xs, result * x)
          }
        }
        iter(self, n.one)
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
      private def appendOne(xs: List[A]): List[A] = {
        ListUtilisesRecursion(this.reverse)
          .foldLeft(xs)((acc, x) => Cons(x, acc))
      }
      def append(lists: List[A]*): List[A] = {
        val ls: List[List[A]] = List(lists: _*)
        appendOne {
          ListUtilisesRecursion(ListUtilisesRecursion(ls).reverse)
            .foldLeft(Nil: List[A]){
              (acc, cur) => ListUtilisesRecursion(cur).appendOne(acc)
            }
        }
      }
    }

    implicit class ListUtilisesFoldLeft[A](self: List[A]) extends ListImplicit[A]{
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
      def sum(implicit n: Numeric[A]): A =
        foldLeft(n.zero)(n.plus)
      def product(implicit n: Numeric[A]): A =
        foldLeft(n.one)(n.times)
      def length: Int =
        foldLeft(0) { (count: Int, _: A) => count + 1 }
      def reverse: List[A] =
        foldLeft(Nil: List[A]) {
          (acc: List[A], x: A) => Cons(x, acc)
        }
      private def appendOne(xs: List[A]): List[A] = {
        ListUtilisesFoldLeft(this.reverse)
          .foldLeft(xs)((acc, x) => Cons(x, acc))
      }
      def append(lists: List[A]*): List[A] = {
        val ls: List[List[A]] = List(lists: _*)
        appendOne {
          ListUtilisesFoldLeft(ListUtilisesFoldLeft(ls).reverse)
            .foldLeft(Nil: List[A]){
              (acc, cur) => ListUtilisesFoldLeft(cur).appendOne(acc)
            }
        }
      }
    }

    implicit class ListUtilisesFoldRight[A](self: List[A]) extends ListImplicit[A] {
      def foldRight[B](default: B)(combine: (A, B) => B): B =
        self match {
          case Nil => default
          case Cons(x, xs) =>
            combine(x,
              ListUtilisesFoldRight(xs)
                .foldRight(default)(combine))
        }
      def sum(implicit n: Numeric[A]): A =
        foldRight(n.zero)(n.plus)
      def product(implicit n: Numeric[A]): A =
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
      private def appendOne(xs: List[A]): List[A] =
        foldRight(xs)(Cons(_, _))
      def append(lists: List[A]*): List[A] = {
        val ls: List[List[A]] = List(lists: _*)
        val acc: List[A] =
          ListUtilisesFoldRight(ls)
            .foldRight(Nil: List[A]){
              (cur, acc) =>
              ListUtilisesFoldRight(cur).appendOne(acc)
            }
        appendOne(acc)
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
