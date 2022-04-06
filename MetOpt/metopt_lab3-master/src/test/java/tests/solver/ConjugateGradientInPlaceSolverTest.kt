package tests.solver

import matrix.DenseMatrix
import matrix.SymmetricSparseRowColumnMatrix
import org.junit.Test
import solver.ConjugateGradientInPlaceSolver
import tests.assertEqualsEps

class ConjugateGradientInPlaceSolverTest {

    val solver = ConjugateGradientInPlaceSolver()
    val eps = 1e-7

    @Test
    fun solve() {
        val matrix = SymmetricSparseRowColumnMatrix(
            DenseMatrix(
                mutableListOf(
                    mutableListOf(1.0, 6.0, 2.0),
                    mutableListOf(6.0, 0.0, 4.0),
                    mutableListOf(2.0, 4.0, 3.0),
                )
            )
        )
        val b = mutableListOf(47.0, 46.0, 47.0)
        val expected = listOf(3.0, 5.0, 7.0)
        val actual = solver.solve(matrix, b, eps)
        assertEqualsEps(expected, actual, eps)
    }
}
