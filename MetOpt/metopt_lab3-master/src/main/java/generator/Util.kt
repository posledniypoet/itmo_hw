package generator

import matrix.Matrix
import util.assertThrow

fun generateB(a: Matrix, x: List<Double>): List<Double> =
    MutableList(x.size) { i ->
        x.mapIndexed { j, value ->
            value * a[i, j]
        }.sum()
    }

fun generateIncrementalVector(n: Int): List<Double> =
    List(n) { it + 1.0 }

operator fun List<Double>.minus(that: List<Double>): List<Double> {
    assertThrow(this.size == that.size, "Incompatible sizes")
    return this.mapIndexed { i, value ->
        value - that[i]
    }
}
