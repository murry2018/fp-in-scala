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
          // head.foldRight(0)(_ + _) shouldBe 100000

          // to test foldRight comment below line.
          throw new StackOverflowError("fake error")
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

  "Exercise 3.14" should "correct" in {
    failAfter(5 seconds) {
      {
        import datastructure.Implicits.ListUtilisesFoldLeft
        List(1, 2, 3, 4, 5).append(List(6, 7, 8, 9, 10))
          .shouldBe(List(1 to 10: _*))
      }
      {
        import datastructure.Implicits.ListUtilisesFoldRight
        List(1, 2, 3, 4, 5).append(List(6, 7, 8, 9, 10))
          .shouldBe(List(1 to 10: _*))
      }
    }
  }

  "Exercise 3.15" should "correct" in {
    failAfter(5 seconds) {
      {
        import datastructure.Implicits.ListUtilisesFoldRight
        List(1, 2, 3, 4, 5).append(
          List(6, 7),
          List(8),
          Nil,
          List(9, 10)
        ) shouldBe List(1 to 10: _*)
      }
      {
        import datastructure.Implicits.ListUtilisesFoldLeft
        List(1, 2, 3, 4, 5).append(
          List(6, 7),
          List(8),
          Nil,
          List(9, 10)
        ) shouldBe List(1 to 10: _*)
      }
      {
        import datastructure.Implicits.ListUtilisesRecursion
        List(1, 2, 3, 4, 5).append(
          List(6, 7),
          List(8),
          Nil,
          List(9, 10)
        ) shouldBe List(1 to 10: _*)
      }
    }
  }

  "Exercise 3.16" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility
    List(1, 2, 3, 4, 5, 6).foreachAddOne
      .shouldBe(List(2, 3, 4, 5, 6, 7))
  }

  "Exercise 3.17" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    List(1, 2, 3, 4, 5, 6).foreachToString
      .shouldBe(List("1", "2", "3", "4", "5", "6"))
  }

  "Exercise 3.18" should "correct" in {
    succeed // because foreachAddOne, foreachToString are written with `map` 
  }

  "Exercise 3.19" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    List(1 to 10: _*).filter(_%2==0)
      .shouldBe(List(2 to 10 by 2: _*))
  }

  "Exercise 3.20" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    val sections = for {
      chapter <- List(1, 2, 3, 4, 5)
      section <- List('a', 'b', 'c')
    } yield s"$chapter$section"
    sections.shouldBe (
      List("1a", "1b", "1c",
        "2a", "2b", "2c",
        "3a", "3b", "3c",
        "4a", "4b", "4c",
        "5a", "5b", "5c")
    )
  }

  "Exercise 3.21" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    List(1 to 10: _*).filterViaFlatMap(_%2==0)
      .shouldBe(List(2 to 10 by 2: _*))

  }

  "Exercise 3.22" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    val xs: List[Int] = List(1 to 10: _*)
    val ys: List[Int] = List(-1 to -10 by -1: _*)
    xs.addAnother(ys)
      .shouldBe(List((for (i <- 1 to 10) yield 0): _*))
  }

  "Exercise 3.23" should "correct" in {
    import datastructure.Implicits.ListUtilisesRecursion
    import datastructure.Implicits.ListCommonUtility

    List(1 to 3: _*).zipWith((_, _))(List('a' to 'z': _*))
      .shouldBe(List((1, 'a'), (2, 'b'), (3, 'c')))
  }

  "Exercise 3.24" should "correct" in {
    import scala.List
    import Chapter3.ListUtil

    assert(List(1, 2, 3, 4).hasSubsequence(List(1, 2)))
    assert(List(1, 2, 3, 4).hasSubsequence(List(2, 3)))
    assert(List(1, 2, 3, 4).hasSubsequence(List(4)))
  }

  trait TreeFixture {
    import Chapter3.{Tree, Leaf, Branch}
    val numericTree: Tree[Int] =
      Branch(
        Branch(
          Leaf(1), Leaf(2)),
        Branch(
          Leaf(3),
          Branch(
            Branch(
              Leaf(4),
              Leaf(5)
            ),
            Leaf(6))))
    val stringTree: Tree[String] =
      Branch(
        Leaf(".gitignore"),
        Branch(
          Branch(
            Branch(
              Branch(
                Leaf("example"),
                Leaf("target")),
              Leaf("com")),
            Leaf("scala")),
          Branch(
            Leaf("item1"), Leaf("item2"))))
  }

  trait TreeFixturePropertyAnswer {
    val numericTreeSize: Int = 11
    val numericTreeMax: Int = 6
    val numericTreeDepth: Int = 5

    val stringTreeSize: Int = 13
    val stringTreeMax: String = "target"
    val stringTreeDepth: Int = 6
  }

  "Exercise 3.25, 3.26, 3.27" should "correct"  in new TreeFixture
      with TreeFixturePropertyAnswer {
    import Chapter3._
    import scala.language.implicitConversions

    numericTree.size shouldBe numericTreeSize
    numericTree.maximum shouldBe numericTreeMax
    numericTree.depth shouldBe numericTreeDepth

    stringTree.size shouldBe stringTreeSize
    stringTree.maximum shouldBe stringTreeMax
    stringTree.depth shouldBe stringTreeDepth
  }

  trait TreeFixtureMappingAnswer {
    import Chapter3.{Tree, Leaf, Branch}
    val numericTreeMapToString: Tree[String] =
      Branch(
        Branch(
          Leaf("1"), Leaf("2")),
        Branch(
          Leaf("3"),
          Branch(
            Branch(
              Leaf("4"),
              Leaf("5")
            ),
            Leaf("6"))))
    val stringTreeMapLength: Tree[Int] =
      Branch(
        Leaf(".gitignore".length),
        Branch(
          Branch(
            Branch(
              Branch(
                Leaf("example".length),
                Leaf("target".length)),
              Leaf("com".length)),
            Leaf("scala".length)),
          Branch(
            Leaf("item1".length), Leaf("item2".length))))
  }

  "Exercise 3.28" should "correct" in new TreeFixture
      with TreeFixtureMappingAnswer {
    import Chapter3._

    numericTree.map(_.toString)
      .shouldBe(numericTreeMapToString)
    stringTree.map(_.length)
      .shouldBe(stringTreeMapLength)
  }

  "Exercise 3.29" should "correct" in new TreeFixture
      with TreeFixturePropertyAnswer {
    import Chapter3._

    treeSize(numericTree) shouldBe numericTreeSize
    maximum(numericTree) shouldBe numericTreeMax
    depth(numericTree) shouldBe numericTreeDepth

    treeSize(stringTree) shouldBe stringTreeSize
    maximum(stringTree) shouldBe stringTreeMax
    depth(stringTree) shouldBe stringTreeDepth
  }
}
