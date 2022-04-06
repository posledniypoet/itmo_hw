import examples.*
import math.ScalarFunction
import math.matrix.Vector
import method.*
import org.junit.jupiter.api.Test
import kotlin.test.assertTrue

class Tests {
    private val METHODS = listOf(
        ClassicNewtonMethod(),
        DescentNewtonMethod(),
        LinearSearchNewtonMethod(),
        PowellMethod(),
        MarquardtMethod(),
        MarquardtCholeskyMethod(),
        DFPMethod()
    )

    private fun abstractTest(
        function: ScalarFunction,
        startPoints: List<Vector> = listOf(
            Vector(listOf(1.0, 1.0)),
            Vector(listOf(5.0, 4.0)),
            Vector(listOf(10.0, 10.0)),
        ),
        inaccuracy: Double = 1e-7,
        methods: List<MinimizeMethod> = METHODS
    ) {
        var assert = false
        for (startPoint in startPoints) {
            val array = ArrayList<Vector>()
            println("\nStart point = $startPoint :")
            for (method in methods) {
                val v = method.minimize(function, startPoint, inaccuracy)
                array.add(v)
                println(method.javaClass.name + ": f{ " + v + " } = " + function.calc(v))
            }


            for (x in array) {
                assert = assert && array[0].equals(x, inaccuracy)
            }
        }

        assertTrue(assert, "Methods give different results")
    }

    @Test
    fun test0() {
        abstractTest(
            F0(),
            listOf(
                Vector(listOf(1.0, 2.0))
            ),
            1e-6
        )
    }

    @Test
    fun test1() {
        abstractTest(
            F1()
        )
    }

    @Test
    fun test2() {
        abstractTest(
            F2()
        )
    }

    @Test
    fun test3() {
        abstractTest(
            F3()
        )
    }

    @Test
    fun test4() {
        abstractTest(
            F4()
        )
    }
}