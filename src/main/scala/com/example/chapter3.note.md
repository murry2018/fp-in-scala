# Chapter 3 Note
이 노트에는 Chapter3 연습문제 중 코드로 작성할 수 없는 답안을 적는다.

## 연습문제 3.1

**[[ Question ]]**

다음 패턴 부합 표현식의 결과는 무엇인가?

```scala

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}
```

**[[ Solution ]]**

`List(1,2,3,4,5)`는 `Cons(x, Cons(y, Cons(3, Cons(4, _))))`에
매칭된다. 따라서 답은 3.

## 연습문제 3.7

**[[ Question ]]**

foldRight로 구현된 product가 0.0을 만났을 때 즉시 재귀를 멈추고 0.0을
돌려줄까? 왜 그럴까? 아니라면 왜 아닐까? foldRight를 긴 목록으로
호출했을 때 어떤 평가 단축이 어떤 식으로 일어나는지 고찰하라. 이는
다른 연습문제보다 심오한 문제이며, 제 5장에서 다시 살펴볼 것이다.

**[[ Solution ]]**

두 가지 이유로 product는 단축 평가되지 않는다. 첫째로 foldRight가
재귀적인 프로세스를 가지도록 정의되어있기 때문이고, 둘째로 곱하기
연산이 인자 먼저 계산하도록(eagerly) 정의되어있기 때문이다.

`product([1, 2, 0, 1, 2])`를 호출한다고 생각해보자.

1. `foldRight([1, 2, 0, 1, 2], 1)(_ * _)`
2. `1 * foldRight([2, 0, 1, 2], 1)(_ * _)`
3. `1 * 2 * foldRight([0, 1, 2], 1)(_ * _)`
4. `1 * 2 * 0 * foldRight([1, 2], 1)(_ * _)`

4번째 재귀 실행과정에서 곱하기 연산은 0을 만났지만, 인자인
`foldRight([1, 2], 1)(_ * _)`가 아직 계산되지 않았기 때문에 이 인자가
모두 평가될 때 까지 기다려야 한다. 설사 여기서 재귀를 멈춘다 하더라도,
아직 `(1 * 2 * 0)` 부분이 평가되지 않은 채 남아있기 때문에 스택을 풀며
되짚어 올라가야 한다.

## 연습문제 3.8

**[[ Question ]]**

`foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))` 처럼 Nil과 Cons
자체를 foldRight에 전달하면 어떤 일이 발생할까? 이로부터, foldRight와
List 자료 생성자들 사이의 관계에 관해 어떤 점을 알 수 있는가?

**[[ Solution ]]**

결과적으로 `List(1, 2, 3)`이 새로 만들어진다. foldRight와 List
생성자를 함께 사용하면 리스트의 원소에 순서대로 연산을 적용하기에 아주
용이해진다.
