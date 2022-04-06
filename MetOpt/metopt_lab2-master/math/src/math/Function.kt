package math

import org.apache.commons.math3.linear.EigenDecomposition
import kotlin.math.abs

class Function(
    val a: Matrix,
    val b: Vector,
    val c: Double,
) {

    fun calc(x: Vector): Double {
        return (x.transposed() * a * x)[0] / 2.0 +
            (b.transposed() * x)[0] + c
    }

    fun grad(x: Vector): Vector {
        return (a * x + b)
    }

    fun rows(): Int {
        return a.matrix.rowDimension
    }

    fun columns(): Int {
        return a.matrix.columnDimension
    }

    fun maxEigenValue(): Double {
        return EigenDecomposition(a.matrix).realEigenvalues.maxOf { abs(it) }
    }
}
