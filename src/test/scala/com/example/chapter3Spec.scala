package com.example

import scala.language.postfixOps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.{
  TimeLimits, Signaler, ThreadSignaler
}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class Chapter3Spec extends AnyFlatSpec with Matchers with TimeLimits {
  import datastructure.{List, Cons, Nil}

  implicit val signaler: Signaler = ThreadSignaler

  "Lists" should "be equal in same representation" in {
    assert(List(1, 2, 3) == Cons(1, Cons(2, Cons(3, Nil))))
    assert(List() == Nil)
  }

  "Exercise 3.1" should "correct" in {
    failAfter (5 seconds) {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      x shouldBe 3
    }
  }

  "Exercise 3.2" should "correct" in {
    failAfter(5 seconds) {
      val to5 = List(1, 2, 3, 4, 5)
      val from0 = Cons(0, to5)
      val only = Cons(0, Nil)
      from0.tail shouldBe to5
      only.tail shouldBe Nil
      only.tail.tail shouldBe Nil
      Nil.tail shouldBe Nil
    }
  }

  "Exercise 3.3" should "correct" in {
    failAfter(5 seconds) {
      val only = Cons(0, Nil)
      only.setHead(4) shouldBe List(4)
      only.tail.setHead("Shit!!!") shouldBe Nil
    }
  }

  "Exercise 3.4" should "correct" in {
    failAfter(5 seconds) {
      val numbers = List((1 to 10): _*)
      numbers.drop(5) shouldBe List(6, 7, 8, 9, 10)
      numbers.drop(10) shouldBe Nil
      numbers.drop(100) shouldBe Nil
      Nil.drop(100) shouldBe Nil
    }
  }

  "Exercise 3.5" should "correct" in {
    failAfter(5 seconds) {
      val isOdd = (_: Int) % 2 != 0
      List(1, 3, 5, 7, 9, 10, 12, 14)
        .dropWhile(isOdd)
        .shouldBe(List(10, 12, 14))
      List(1, 3, 5, 7, 9)
        .dropWhile(isOdd)
        .shouldBe(Nil)
      List()
        .dropWhile(isOdd)
        .shouldBe(Nil)
    }
  }

  "Exercise 3.6" should "correct" in {
    failAfter(5 seconds) {
      List(1,2,3,4,5).init
        .shouldBe(List(1, 2, 3, 4))
      List(5).init shouldBe Nil
      Nil.init shouldBe Nil
    }
  }
}
