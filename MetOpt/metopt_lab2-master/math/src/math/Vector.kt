package math

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.RealMatrix
import java.lang.Double.sum
import java.util.function.BinaryOperator
import java.util.function.IntFunction
import java.util.stream.IntStream

class Vector(
    val matrix: RealMatrix,
) {

    constructor(array: DoubleArray) : this(Array2DRowRealMatrix(array))

    fun transposed(): Matrix = Matrix(
        Array2DRowRealMatrix(
            arrayOf(matrix.getColumn(0))
        )
    )

    fun abs(): Double {
        return matrix.getColumn(0).sumOf { it * it }
    }

    operator fun times(arg: Matrix): Matrix {
        return Matrix(matrix.multiply(arg.matrix))
    }

    operator fun times(arg: Vector): Matrix {
        return Matrix(matrix.multiply(arg.matrix))
    }

    operator fun times(arg: Double): Vector {
        return Vector(matrix.scalarMultiply(arg))
    }

    operator fun minus(arg: Vector): Vector {
        return Vector(matrix.subtract(arg.matrix))
    }

    operator fun minus(arg: Matrix): Vector {
        return Vector(matrix.subtract(arg.matrix))
    }

    operator fun get(index: Int): Double {
        return matrix.getEntry(index, 0)
    }

    override fun toString(): String {
        return "$matrix"
    }

    operator fun plus(x: Vector): Vector {
        return Vector(this.matrix.add(x.matrix))
    }

    fun scalar(x: Vector): Double {
        return (0 until matrix.rowDimension).sumOf { this[it] * x[it] }
    }

    fun size(): Int {
        return matrix.rowDimension
    }
}
