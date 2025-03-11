package u02

import org.junit.*
import org.junit.Assert.*
import Lab2.*

class ExprTest:
  val SIMPLE_FIRST_INTEGER_COSTANT = 5
  val SIMPLE_SECOND_INTEGER_COSTANT = 10
  val COMPLEX_FIRST_EXP: Int = (SIMPLE_FIRST_INTEGER_COSTANT + SIMPLE_SECOND_INTEGER_COSTANT) * SIMPLE_FIRST_INTEGER_COSTANT
  val COMPLEX_SECOND_EXP: Int = SIMPLE_FIRST_INTEGER_COSTANT * SIMPLE_SECOND_INTEGER_COSTANT
  val SUM_STRING: String = "(" + SIMPLE_FIRST_INTEGER_COSTANT.toString + " + " + SIMPLE_SECOND_INTEGER_COSTANT.toString + ")"
  val MULT_STRING: String = "(" + SIMPLE_FIRST_INTEGER_COSTANT.toString + " * " + SIMPLE_SECOND_INTEGER_COSTANT.toString + ")"
  val COMPLEX_SUM_STRING: String = "(" + COMPLEX_FIRST_EXP.toString + " + " + COMPLEX_SECOND_EXP.toString + ")"
  val COMPLEX_MULT_STRING: String = "(" + COMPLEX_FIRST_EXP.toString + " * " + COMPLEX_SECOND_EXP.toString + ")"

  @Test def testSimpleAdd(): Unit =
    val exp1: Expr = Expr.Literal(SIMPLE_FIRST_INTEGER_COSTANT)
    val exp2: Expr = Expr.Literal(SIMPLE_SECOND_INTEGER_COSTANT)
    val sum: Expr = Expr.Add(exp1, exp2)
    assertEquals(SIMPLE_FIRST_INTEGER_COSTANT + SIMPLE_SECOND_INTEGER_COSTANT, evaluate(sum))
    assertEquals(SUM_STRING, show(sum))

  @Test def testSimpleMultiply(): Unit =
    val exp1: Expr = Expr.Literal(SIMPLE_FIRST_INTEGER_COSTANT)
    val exp2: Expr = Expr.Literal(SIMPLE_SECOND_INTEGER_COSTANT)
    val mult: Expr = Expr.Multiply(exp1, exp2)
    assertEquals(SIMPLE_FIRST_INTEGER_COSTANT * SIMPLE_SECOND_INTEGER_COSTANT, evaluate(mult))
    assertEquals(MULT_STRING, show(mult))

  @Test def testComplexAdd(): Unit =
    val exp1: Expr = Expr.Literal(COMPLEX_FIRST_EXP)
    val exp2: Expr = Expr.Literal(COMPLEX_SECOND_EXP)
    val sum: Expr = Expr.Add(exp1, exp2)
    assertEquals(COMPLEX_FIRST_EXP + COMPLEX_SECOND_EXP, evaluate(sum))
    assertEquals(COMPLEX_SUM_STRING, show(sum))

  @Test def testComplexMultiply(): Unit =
    val exp1: Expr = Expr.Literal(COMPLEX_FIRST_EXP)
    val exp2: Expr = Expr.Literal(COMPLEX_SECOND_EXP)
    val mult: Expr = Expr.Multiply(exp1, exp2)
    assertEquals(COMPLEX_FIRST_EXP * COMPLEX_SECOND_EXP, evaluate(mult))
    assertEquals(COMPLEX_MULT_STRING, show(mult))