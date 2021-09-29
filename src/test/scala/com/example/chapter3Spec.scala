package com.example

import scala.language.postfixOps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.{
  TimeLimits, Signaler, ThreadSignaler
}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class Chapter3Spec extends AnyFlatSpec with Matchers with TimeLimits {
  import datastructure._

  implicit val signaler: Signaler = ThreadSignaler

  "Lists" should "be equal in same representation" in {
    assert(List(1, 2, 3) == Cons(1, Cons(2, Cons(3, Nil))))
    assert(List() == Nil)
  }

  "List operation" should "be covariant for its argument" in {
    abstract class Animal
    case class Cat(n: Int) extends Animal
    case class Dog(n: Int) extends Animal

    val cats: List[Cat] = List(new Cat(1), new Cat(2), new Cat(3))
    val animals: List[Animal] = cats
    assert(animals.setHead(new Dog(0)).isInstanceOf[List[Animal]])
  }

  "Numeric list operations" should "not be applied to non-numeric value" in {
    assertCompiles("""
      |import com.example.datastructure.{List, Cons, Nil}
      |import com.example.datastructure.Implicits.ListUtilisesFoldRight
      |List(1, 2, 3, 4, 5).sum
      |""".stripMargin)

    assertTypeError("""
      |import com.example.datastructure.{List, Cons, Nil}
      |import com.example.datastructure.Implicits.ListUtilisesFoldRight
      |List("t", "e", "s", "t").sum
      |""".stripMargin)
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

  "Exercise 3.9" should "correct" in {
    failAfter(5 seconds) {
      import datastructure.Implicits.ListUtilisesFoldRight
      List().length shouldBe 0
      List(1).length shouldBe 1
      List(1, 2, 3).length shouldBe 3
      List(1, 2, 3, 4, 5).length shouldBe 5
    }
  }

  "Exercise 3.10" should "correct" in {
    failAfter(10 seconds) {
      var head: List[Int] = Nil
      for (i <- 1 to 100000) {
        head = Cons(1, head)
      }
      {
        import datastructure.Implicits.ListUtilisesFoldRight
        intercept[StackOverflowError] {
          // to test foldRight uncomment below line.
          head.foldRight(0)(_ + _) shouldBe 100000

          // to test foldRight comment below line.
          // throw new StackOverflowError("fake error")
        }
      }
      {
        import datastructure.Implicits.ListUtilisesFoldLeft
        head.foldLeft(0)(_ + _) shouldBe 100000
      }
    }
  }

  "Exercise 3.11" should "correct" in {
    failAfter(5 seconds) {
      import datastructure.Implicits.ListUtilisesFoldLeft
      var head: List[Int] = Nil
      for (i <- 1 to 100000) {
        head = Cons(1, head)
      }
      head.sum shouldBe 100000
      head.product shouldBe 1
      head.length shouldBe 100000
    }
  }

  "Exercise 3.12" should "correct" in {
    failAfter(5 seconds) {
      {
        import datastructure.Implicits.ListUtilisesRecursion
        List(1, 2, 3, 4, 5).reverse
          .shouldBe(List(5, 4, 3, 2, 1))
        Nil.reverse
          .shouldBe(Nil)
      }
      {
        import datastructure.Implicits.ListUtilisesFoldLeft
        List(1, 2, 3, 4, 5).reverse
          .shouldBe(List(5, 4, 3, 2, 1))
        Nil.reverse
          .shouldBe(Nil)
      }
      {
        import datastructure.Implicits.ListUtilisesFoldRight
        List(1, 2, 3, 4, 5).reverse
          .shouldBe(List(5, 4, 3, 2, 1))
        Nil.reverse
          .shouldBe(Nil)
      }
    }
  }

  "Exercise 3.13" should "correct" in {
    failAfter(5 seconds) {
      { // implement foldRight via foldLeft
        import datastructure.Implicits.ListUtilisesFoldLeft
        List(1, 2, 3, 4, 5).foldRight(Nil: List[Int])(Cons(_, _))
          .shouldBe(List(1, 2, 3, 4, 5))
      }
      { // implement foldLeft via foldRight
        import datastructure.Implicits.ListUtilisesFoldRight
        List(1, 2, 3, 4, 5).foldLeft(Nil: List[Int])((acc, x) => Cons(x, acc))
          .shouldBe(List(5, 4, 3, 2, 1))
      }
      { // stack-safe foldRight
        import datastructure.Implicits.ListUtilisesFoldLeft
        var head: List[Int] = Nil
        for (i <- 1 to 100000) {
          head = Cons(1, head)
        }
        head.foldRight(0)(_ + _) shouldBe 100000
      }
    }
  }
}
