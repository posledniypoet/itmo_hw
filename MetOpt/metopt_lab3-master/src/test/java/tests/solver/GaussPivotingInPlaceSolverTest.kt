package tests.solver

import matrix.DenseMatrix
import org.junit.Test
import solver.GaussPivotingInPlaceSolver
import tests.assertEqualsEps

class GaussPivotingInPlaceSolverTest {

    val solver = GaussPivotingInPlaceSolver()
    val eps = 1e-7

    @Test
    fun solve() {
        val matrix = DenseMatrix(
            mutableListOf(
                mutableListOf(4.0, 1.0, 1.0),
                mutableListOf(1.0, 6.0, -1.0),
                mutableListOf(1.0, 2.0, 5.0),
            )
        )
        val b = mutableListOf(9.0, 10.0, 20.0)
        val expected = listOf(1.0, 2.0, 3.0)
        val actual = solver.solve(matrix, b, eps)
        assertEqualsEps(expected, actual, eps)
    }
}
