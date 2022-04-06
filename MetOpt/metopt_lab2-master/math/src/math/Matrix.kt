package math

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.EigenDecomposition
import org.apache.commons.math3.linear.RealMatrix

class Matrix(
    val matrix: RealMatrix,
) {

    constructor(array: Array<DoubleArray>) : this(Array2DRowRealMatrix(array))

    fun getEigenValues(): DoubleArray = EigenDecomposition(matrix).realEigenvalues

    fun abs(): Double {
        return matrix.data
            .flatMap { arr -> arr.map { it * it } }
            .sum()
    }

    operator fun times(vector: Vector): Vector {
        return Vector(matrix.multiply(vector.matrix))
    }

    operator fun times(arg: Matrix): Matrix {
        return Matrix(matrix.multiply(arg.matrix))
    }

    operator fun times(arg: Double): Matrix {
        return Matrix(matrix.scalarMultiply(arg))
    }

    operator fun get(row: Int, col: Int): Double {
        return matrix.getEntry(row, col)
    }

    operator fun minus(arg: Vector): Matrix {
        return Matrix(matrix.subtract(arg.matrix))
    }

    operator fun minus(arg: Matrix): Matrix {
        return Matrix(matrix.subtract(arg.matrix))
    }

    override fun toString(): String {
        return "$matrix"
    }
}
