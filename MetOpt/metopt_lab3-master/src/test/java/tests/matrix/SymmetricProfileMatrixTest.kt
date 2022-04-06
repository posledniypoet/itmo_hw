package tests.matrix

import matrix.DenseMatrix
import matrix.SymmetricProfileMatrix
import tests.assertEqualsEps

import org.junit.Test

class SymmetricProfileMatrixTest {

    val eps = 1e-7

    @Test
    fun test1() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 2.0, 3.0),
                mutableListOf(4.0, 5.0, 6.0),
                mutableListOf(7.0, 8.0, 9.0),
            )
        )
        val symmetricProfileMatrix = SymmetricProfileMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricProfileMatrix, eps)
    }

    @Test
    fun test2() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 0.0, 0.0),
                mutableListOf(0.0, 5.0, 0.0),
                mutableListOf(0.0, 0.0, 9.0),
            )
        )
        val symmetricProfileMatrix = SymmetricProfileMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricProfileMatrix, eps)
    }

    @Test
    fun test3() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 0.0, 1.0),
                mutableListOf(0.0, 5.0, 0.0),
                mutableListOf(11.0, 0.0, 9.0),
            )
        )
        val symmetricProfileMatrix = SymmetricProfileMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricProfileMatrix, eps)
    }

    @Test
    fun test4() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 0.0, 1.0),
                mutableListOf(0.0, 0.0, 0.0),
                mutableListOf(11.0, 0.0, 9.0),
            )
        )
        val symmetricProfileMatrix = SymmetricProfileMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricProfileMatrix, eps)
    }
}
