package com.example

import scala.language.postfixOps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.{
  TimeLimits, Signaler, ThreadSignaler
}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class Chapter2Spec extends AnyFlatSpec with Matchers with TimeLimits {
  import Chapter2._

  implicit val signaler: Signaler = ThreadSignaler

  "Exercise 2.1" should "Correct" in {
    failAfter(5 seconds) {
      fib(0) shouldBe 0
      fib(1) shouldBe 1
      fib(2) shouldBe 1
      fib(3) shouldBe 2
      fib(4) shouldBe 3
      fib(5) shouldBe 5
    }
  }

  "Exercise 2.2" should "Correct" in {
    failAfter(5 seconds) {
      val orderedAsNumber = {
        (ns1: String, ns2: String) =>
        (ns1.toIntOption, ns2.toIntOption) match {
          case (Some(n1), Some(n2)) => n1 < n2
          case _ => ns1 < ns2
        }
      }
      orderedAsNumber("12", "2") shouldBe false
      orderedAsNumber("3", "10") shouldBe true
      isSorted(Array(1, 2, 3, 4, 5), _ < _) shouldBe true
      isSorted(Array("e", "lo", "love", "ov", "ve"), _ < _) shouldBe true
      isSorted(Array("1", "10", "12", "2", "3"), orderedAsNumber) shouldBe false
      isSorted(Array("1", "2", "3", "10", "12"), orderedAsNumber) shouldBe true
    }
  }

  "Exercise 2.3" should "Correct" in {
    failAfter(5 seconds) {
      val curriedAdd = curry((_: Int) + (_: Int))
      curriedAdd(3)(4) shouldBe (3 + 4)
    }
  }

  "Exercise 2.4" should "Correct" in {
    failAfter(5 seconds) {
      val curriedAdd = curry((_: Int) + (_: Int))
      uncurry(curriedAdd)(3, 4) shouldBe (3 + 4)
    }
  }

  "Exercise 2.5" should "Correct" in {
    failAfter(5 seconds) {
      compose((_: Int)*3, (_: Int)+2)(1) shouldBe ((1+2)*3)
    }
  }
}
