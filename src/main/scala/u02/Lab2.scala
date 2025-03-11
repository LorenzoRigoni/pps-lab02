package u02

import u02.Lab2.Expr.{Add, Literal}

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

  //Task 3
  println("Task 3.7: recursion power")

  def power (base: Double, exp: Int): Double = exp match
    case 0.0 => 1.0
    case _ => base * power(base, exp - 1)

  println("Expected '8.0' with value 2 and 3, result: " + power(2, 3))
  println("Expected '25.0' with value 2 and 3, result: " + power(5, 2))
  println()

  def tailPower (base: Double, exp: Int): Double =
    @annotation.tailrec
    def _pow(exp: Int, acc: Double): Double = exp match
      case 0.0 => acc
      case _ => _pow(exp - 1, base * acc)
    _pow(exp, 1)

  println("Expected '8.0' with value 2 and 3, result: " + tailPower(2, 3))
  println("Expected '25.0' with value 2 and 3, result: " + tailPower(5, 2))
  println()

  println("Task 3.8: reverse function")

  def reverseNumber (n: Int): Int =
    @annotation.tailrec
    def _rev(rem: Int, acc: Int, pow: Int): Int = rem match
      case 0 => acc
      case _ => _rev(rem / 10, acc + ((rem % 10) * pow), pow / 10)
    _rev(n, 0, math.pow(10, floor(log10(n))).toInt)

  println("Expected '54321' with value 12345, result: " + reverseNumber(12345))
  println()

  //Task 4
  println("Task 4.9: sum types")

  enum Expr:
    case Literal (const: Int)
    case Add (exp1: Expr, exp2: Expr)
    case Multiply (exp1: Expr, exp2: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Literal(e) => e
    case Expr.Add(e1, e2) => evaluate(e1) + evaluate(e2)
    case Expr.Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

  def show (expr: Expr): String = expr match
    case Expr.Literal(e) => e.toString
    case Expr.Add(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
    case Expr.Multiply(e1, e2) => "(" + show(e1) + " * " + show(e2) + ")"

  //In test/scala/u02 you can find the tests about Expr