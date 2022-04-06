package solver

import matrix.LWrapperMatrix
import matrix.Matrix
import matrix.UWrapperMatrix

class LUInPlaceSolver : Solver<Matrix> {

    fun decompose(matrix: Matrix): Pair<UWrapperMatrix, LWrapperMatrix> {
        (0 until matrix.size()).forEach { x ->
            (x until matrix.size()).forEach { j ->
                matrix[x, j] -= (0 until x).sumOf { pX ->
                    matrix[x, pX] * matrix[pX, j]
                }
            }
            if (matrix[x, x] == 0.0) {
                throw IllegalArgumentException("Zero on diagonal")
            }
            (x + 1 until matrix.size()).forEach { i ->
                matrix[i, x] -= (0 until x).sumOf { pX ->
                    matrix[i, pX] * matrix[pX, x]
                }
                matrix[i, x] /= matrix[x, x]
            }
        }
        return UWrapperMatrix(matrix) to LWrapperMatrix(matrix)
    }

    override fun solve(matrix: Matrix, b: MutableList<Double>, eps: Double): List<Double> {
        val (u, l) = decompose(matrix)

        (0 until matrix.size()).forEach { i ->
            b[i] -= (0 until i).sumOf { j ->
                b[j] * l[i, j]
            }
        }

        (matrix.size() - 1 downTo 0).forEach { i ->
            b[i] -= (i + 1 until matrix.size()).sumOf { j ->
                b[j] * u[i, j]
            }
            b[i] /= u[i, i]
        }

        return b
    }
}
