package application

import firstorder.MethodEnum
import firstorder.AbstractFirstOrder
import linear.LinearMethodEnum
import math.Function
import math.Matrix
import math.Vector
import org.junit.Test
import kotlin.test.assertTrue

class Test {

    private fun test01(method: MethodEnum) {
        val function = Function(
            Matrix(arrayOf(doubleArrayOf(2.0, 0.0), doubleArrayOf(0.0, 8.0))),
            Vector(doubleArrayOf(4.0, 2.0)),
            0.0
        )
        val vector = Vector(doubleArrayOf(1.0, 1.0))

        val eps = 0.000001
        val expected = Vector(doubleArrayOf(-2.0, -0.25))
        val actual = AbstractFirstOrder()
            .minimize(method, function, vector, eps, 10.0, LinearMethodEnum.GOLDEN_RATIO)
        assertTrue(
            function.calc(expected) - actual.second < eps,
            "Expected: ${expected to function.calc(expected)}, Actual: $actual"
        )
    }

    @Test
    fun test01GradientDescent() {
        test01(MethodEnum.GRADIENT_DESCENT)
    }

    @Test
    fun test01FastDescent() {
        test01(MethodEnum.FAST_DESCENT)
    }

    @Test
    fun test01ConjugateGradient() {
        test01(MethodEnum.CONJUGATE_GRADIENT)
    }
}
