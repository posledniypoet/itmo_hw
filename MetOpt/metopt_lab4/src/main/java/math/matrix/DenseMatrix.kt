package math.matrix

import assertThrow

class DenseMatrix(
    val matrix: MutableList<MutableList<Double>>
) : Matrix {

    override fun get(i: Int, j: Int): Double =
        matrix[i][j]

    override fun set(i: Int, j: Int, value: Double) {
        matrix[i][j] = value
    }

    override fun size(): Int =
        matrix.size

    fun swapRows(i: Int, j: Int) {
        matrix[i] = matrix[j].also { matrix[j] = matrix[i] }
    }

    operator fun times(v: List<Double>): List<Double> {
        assertThrow(v.size == size(), "Incompatible sizes")
        return (v.indices).map { i ->
            v.mapIndexed { j, value ->
                matrix[i][j] * value
            }.sum()
        }
    }

    operator fun times(d: Double): DenseMatrix {
        matrix.indices.map { i ->
            matrix[i].indices.map { j ->
                matrix[i][j] *= d
            }
        }
        return this
    }

    operator fun plus(that: Matrix): DenseMatrix =
        DenseMatrix(
            matrix.indices.map { i ->
                matrix[i].indices.map { j ->
                    this[i, j] + that[i, j]
                }.toMutableList()
            }.toMutableList()
        )

    operator fun plusAssign(that: Matrix) {
        assertThrow(that.size() == size(), "Incompatible sizes")
        matrix.indices.map { i ->
            matrix[i].indices.map { j ->
                matrix[i][j] += that[i, j]
            }
        }
    }

    operator fun minusAssign(that: Matrix) {
        assertThrow(that.size() == size(), "Incompatible sizes")
        matrix.indices.map { i ->
            matrix[i].indices.map { j ->
                matrix[i][j] -= that[i, j]
            }
        }
    }

    operator fun times(helper: Vector): Vector {
        return Vector(this * helper.data())
    }
}

fun identityMatrix(size: Int): DenseMatrix {
    return DenseMatrix(
        MutableList(size) { i ->
            MutableList(size) { j ->
                if (i == j) {
                    1.0
                } else {
                    0.0
                }
            }
        }
    )
}