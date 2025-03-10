package u02

import scala.math.*

object Lab2 extends App:
  //Task 2.3

  //a: positive function
  println("Task 2.3")
  println("a: positive function")

  println("Lambda function")
  val literalPositive: Int => String = x => x match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("Expected 'positive' with value 100, result: " + literalPositive(100))
  println("Expected 'negative' with value -100, result: " + literalPositive(-100))
  println()

  println("Method syntax")
  def positiveMethod(x: Int): String = x match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("Expected 'positive' with value 100, result: " + literalPositive(100))
  println("Expected 'negative' with value -100, result: " + literalPositive(-100))
  println()

  //b: neg function String => Boolean
  println("b: neg function")

  def neg(predicate: String => Boolean): String => Boolean = (v: String) => !predicate(v)

  val empty: String => Boolean = _ == ""
  val notEmpty = neg(empty)
  println("Expected 'true' with value 'foo', result: " + notEmpty("foo"))
  println("Expected 'false' with value '', result: " + notEmpty(""))
  println("Expected 'true' with value 'foo' and '', result: " + (notEmpty("foo") && !notEmpty("")))
  println()

  //c: generic neg function
  println("c: generic neg function")

  def genericNeg[A](predicate: A => Boolean): A => Boolean = (v: A) => !predicate(v)

  val isFive: Int => Boolean = _ == 5
  val isNotFive = genericNeg(isFive)
  println("Expected 'true' with value '12', result: " + isNotFive(12))
  println("Expected 'false' with value '5', result: " + isNotFive(5))
  println("Expected 'true' with value '12' and '5', result: " + (isNotFive(12) && !isNotFive(5)))
  println()

  //Task 2.4
  println("Task 2.4: currying")

  val p1: Int => (Int, Int) => Boolean = x => (y, z) => (x <= y) && (y == z)
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => (x <= y) && (y == z)
  def p3(x: Int)(y: Int, z: Int): Boolean = (x <= y) && (y == z)
  def p4(x: Int, y: Int, z: Int): Boolean = (x <= y) && (y == z)

  println("Expected 'true' with value 5, 10, 10 to p1 in form (5)(10, 10), result: " + p1(5)(10, 10))
  println("Expected 'true' with value 5, 10, 10 to p2 in form (5, 10, 10), result: " + p2(5, 10, 10))
  println("Expected 'true' with value 5, 10, 10 to p3 in form (5)(10, 10), result: " + p3(5)(10, 10))
  println("Expected 'true' with value 5, 10, 10 to p4 in form (5, 10, 10), result: " + p4(5, 10, 10))
  println()

  //Task 2.5
  println("Task 2.5: 2 functional compositions")

  def compose[A, B, C] (f: B => A, g: C => B): C => A = (x: C) => f(g(x))

  println("Expected '9' with value a - 1, a * 2, 5, result: " + compose((a: Int) => a - 1, (a: Int) => a * 2)(5))
  println()

  //Task 2.6
  println("Task 2.6: 3 functional compositions")

  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = (x: A) => f(g(h(x)))

  println("Expected '6!' with value a + !, a.ToString, a * 2, 3, result: " + composeThree((a: String) => a + "!", (a: Int) => a.toString, (a: Int) => a * 2)(3))
  println()