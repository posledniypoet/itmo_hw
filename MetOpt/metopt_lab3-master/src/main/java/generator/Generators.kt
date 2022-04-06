package generator

import matrix.DenseMatrix
import matrix.SymmetricSparseRowColumnMatrix
import kotlin.math.pow
import kotlin.random.Random

val dis = { Random.nextInt(-4, 1).toDouble() }
val disNonZero = { Random.nextInt(-4, 0).toDouble() }

fun generateDiagonallyDominantMatrix(n: Int, k: Int): DenseMatrix {
    while (true) {
        val a = MutableList(n) {
            MutableList(n) { 0.0 }
        }
        a[0][0] = 10.0.pow(-k.toDouble())

        (0 until n).map { i ->
            (0 until i).map { j ->
                a[i][j] = dis()
                if (a[i][j] != 0.0) {
                    a[j][i] = disNonZero()
                }
            }
        }

        (0 until n).map { i ->
            (0 until n).map { j ->
                if (i != j) {
                    a[i][i] -= a[i][j]
                }
            }
        }

        if ((0 until n).all { i -> a[i][i] != 0.0 }) {
            return DenseMatrix(a)
        }
    }
}

fun generateHilbertMatrix(n: Int): DenseMatrix =
    DenseMatrix(
        (0 until n).map { i ->
            (0 until n).map { j ->
                1.0 / (i + j + 1)
            }.toMutableList()
        }.toMutableList()
    )

fun generateSymmetricDiagonallyDominantMatrix(n: Int, invert: Boolean): SymmetricSparseRowColumnMatrix {
    val result = SymmetricSparseRowColumnMatrix(n)
    result[0, 0] = 1.0

    (0 until n).map { i ->
        val line = MutableList(i) { 0.0 }
        (0 until i).map { j ->
            val x = if (invert) -dis() else dis()
            line[j] = x
            result[i, i] -= x
            result[j, j] -= x
        }
        result.insertLine(line)
    }

    return result
}

fun generateSymmetricDiagonallyDominantDenseMatrix(n: Int, invert: Boolean): DenseMatrix {
    val a = MutableList(n) {
        MutableList(n) { 0.0 }
    }
    a[0][0] = 1.0

    (0 until n).map { i ->
        (0 until i).map { j ->
            val x = if (invert) -dis() else dis()
            a[i][j] = x
            a[j][i] = x
        }
    }

    (0 until n).map { i ->
        (0 until n).map { j ->
            if (i != j) {
                a[i][i] -= a[i][j]
            }
        }
    }

    return DenseMatrix(a)
}
