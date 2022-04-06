package examples

import math.ScalarFunction
import math.matrix.DenseMatrix
import math.matrix.Vector
import kotlin.math.pow

class F0 : ScalarFunction() {
    override fun calc(vector: Vector): Double {
        return 50 * vector.data()[0] * vector.data()[0] +
                vector.data()[1] * vector.data()[1] +
                20 * vector.data()[0] + 20 * vector.data()[1] + 239
    }

    override fun gradient(point: Vector): Vector {
        return Vector(
            listOf(
                20 * (5 * point.data()[0] + 1),
                2 * (point.data()[1] + 10)
            )
        )
    }

    override fun hessian(point: Vector): DenseMatrix {
        return DenseMatrix(arrayListOf(
            arrayListOf(100.0, 0.0),
            arrayListOf(0.0, 2.0)
        ))
    }

}

class F1 : ScalarFunction() {
    override fun calc(vector: Vector): Double {
        val m1: Double = vector.data()[1] - vector.data()[0] * vector.data()[0]
        val m2: Double = 1.0 - vector.data()[0]
        return 100.0 * m1 * m1 + m2 * m2
    }

    override fun gradient(point: Vector): Vector {
        return Vector(
            listOf(
                -2.0 * (1.0 + -1.0 * point.data()[0]) +
                        -400.0 * point.data()[0] * (-1.0 * point.data()[0].pow(2.0) + point.data()[1]),
                200.0 * (-1.0 * point.data()[0].pow(2.0) + point.data()[1])
            )
        )
    }

    override fun hessian(point: Vector): DenseMatrix {
        return DenseMatrix(
            arrayListOf(
                arrayListOf(
                    2.0 + 800.0 * point.data()[0].pow(2.0) +
                            -400.0 * (-1.0 * point.data()[0].pow(2.0) + point.data()[1]),
                    -400.0 * point.data()[0]
                ),
                arrayListOf(-400.0 * point.data()[0], 200.0)
            )
        )
    }
}


class F2 : ScalarFunction() {
    override fun calc(vector: Vector): Double {
        return vector.data()[0] * vector.data()[0] +
                vector.data()[1] * vector.data()[1] -
                1.2 * vector.data()[0] * vector.data()[1]
    }

    override fun gradient(point: Vector): Vector {
        return Vector(
            listOf(
                2 * point.data()[0] - 1.2 * point.data()[1],
                2 * point.data()[1] - 1.2 * point.data()[0]
            )
        )
    }

    override fun hessian(point: Vector): DenseMatrix {
        return DenseMatrix(
            arrayListOf(
                arrayListOf(
                    2.0, -1.2
                ),
                arrayListOf(
                    -1.2, 2.0
                )
            )
        )
    }
}

class F3 : ScalarFunction() {
    override fun calc(vector: Vector): Double {
        return vector.data()[0] * vector.data()[0] +
                4 * vector.data()[1] * vector.data()[1] +
                2 * vector.data()[0] * vector.data()[1]
    }

    override fun gradient(point: Vector): Vector {
        return Vector(
            listOf(
                ((2.0 * point.data()[0]) + (2.0 * point.data()[1])),
                ((2.0 * point.data()[0]) + (8.0 * point.data()[1]))
            )
        )
    }

    override fun hessian(point: Vector): DenseMatrix {
        return DenseMatrix(
            arrayListOf(
                arrayListOf(
                    2.0, 2.0
                ),
                arrayListOf(
                    2.0, 8.0
                )
            )
        )
    }
}


class F4 : ScalarFunction() {
    override fun calc(vector: Vector): Double {
        return 0.25 * (vector.data()[0] * vector.data()[0] * vector.data()[0] * vector.data()[0]) +
                (vector.data()[1] * vector.data()[1] * vector.data()[1] * vector.data()[1]) +
                (vector.data()[0] * vector.data()[0])
    }

    override fun gradient(point: Vector): Vector {
        return Vector(
            listOf(
                point.data()[0] * point.data()[0] * point.data()[0] + 2 * point.data()[0],
                4 * point.data()[1] * point.data()[1] * point.data()[1]
            )
        )
    }

    override fun hessian(point: Vector): DenseMatrix {
        return DenseMatrix(
            arrayListOf(
                arrayListOf(
                    3 * point.data()[0] * point.data()[0] + 2, 0.0
                ),
                arrayListOf(
                    0.0, 12 * point.data()[1] * point.data()[1]
                )
            )
        )
    }
}