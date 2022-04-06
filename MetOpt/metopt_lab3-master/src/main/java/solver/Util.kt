package solver

import util.assertThrow
import kotlin.math.sqrt

operator fun List<Double>.plus(that: List<Double>): List<Double> {
    assertThrow(this.size == that.size, "Incompatible sizes")
    return this.mapIndexed { i, value ->
        value + that[i]
    }
}

operator fun List<Double>.minus(that: List<Double>): List<Double> {
    assertThrow(this.size == that.size, "Incompatible sizes")
    return this.mapIndexed { i, value ->
        value - that[i]
    }
}

operator fun List<Double>.times(d: Double): List<Double> =
    this.map { it * d }

operator fun List<Double>.times(that: List<Double>): Double {
    assertThrow(this.size == that.size, "Incompatible sizes")
    return this.mapIndexed { i, value ->
        value * that[i]
    }.sum()
}

fun List<Double>.abs(): Double =
    sqrt(this.sumOf { it * it })
