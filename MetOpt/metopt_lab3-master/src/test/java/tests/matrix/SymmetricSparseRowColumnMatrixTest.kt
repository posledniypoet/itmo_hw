package tests.matrix

import matrix.DenseMatrix
import matrix.SymmetricSparseRowColumnMatrix
import org.junit.Test
import tests.assertEqualsEps

class SymmetricSparseRowColumnMatrixTest {

    val eps = 1e-7

    @Test
    fun test1() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 2.0, 3.0, 4.0),
                mutableListOf(2.0, 6.0, 7.0, 8.0),
                mutableListOf(3.0, 7.0, 11.0, 12.0),
                mutableListOf(4.0, 8.0, 12.0, 16.0),
            )
        )
        val symmetricSparseRowColumnMatrix = SymmetricSparseRowColumnMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricSparseRowColumnMatrix, eps)
    }

    @Test
    fun test2() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 0.0, 0.0),
                mutableListOf(0.0, 5.0, 5.0),
                mutableListOf(0.0, 5.0, 9.0),
            )
        )
        val symmetricSparseRowColumnMatrix = SymmetricSparseRowColumnMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricSparseRowColumnMatrix, eps)
    }

    @Test
    fun test3() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 2.0, 3.0, 4.0),
                mutableListOf(2.0, 6.0, 0.0, 0.0),
                mutableListOf(3.0, 0.0, 11.0, 2.0),
                mutableListOf(4.0, 0.0, 2.0, 16.0),
            )
        )
        val symmetricSparseRowColumnMatrix = SymmetricSparseRowColumnMatrix(denseMatrix)
        assertEqualsEps(denseMatrix, symmetricSparseRowColumnMatrix, eps)
    }
}
