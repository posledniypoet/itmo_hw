package tests.solver

import matrix.DenseMatrix
import matrix.SymmetricProfileMatrix
import org.junit.Test
import solver.LUInPlaceSolver
import tests.assertEqualsEps

class LUInPlaceSolverTest {

    val solver = LUInPlaceSolver()
    val eps = 1e-7

    @Test
    fun decompose() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(4.0, 1.0, 1.0),
                mutableListOf(1.0, 6.0, -1.0),
                mutableListOf(1.0, 2.0, 5.0),
            )
        )
        val uExpected = DenseMatrix(
            mutableListOf(
                mutableListOf(4.0, 1.0, 1.0),
                mutableListOf(0.0, 23.0 / 4.0, -5.0 / 4.0),
                mutableListOf(0.0, 0.0, 118.0 / 23.0),
            )
        )
        val lExpected = DenseMatrix(
            mutableListOf(
                mutableListOf(1.0, 0.0, 0.0),
                mutableListOf(1.0 / 4.0, 1.0, 0.0),
                mutableListOf(1.0 / 4.0, 7.0 / 23.0, 1.0),
            )
        )

        val (uActual, lActual) = solver.decompose(denseMatrix)

        assertEqualsEps(uExpected, uActual, eps)
        assertEqualsEps(lExpected, lActual, eps)
    }

    @Test
    fun solve() {
        val denseMatrix = DenseMatrix(
            mutableListOf(
                mutableListOf(4.0, 1.0, 1.0),
                mutableListOf(1.0, 6.0, -1.0),
                mutableListOf(1.0, 2.0, 5.0),
            )
        )
        val b = mutableListOf(9.0, 10.0, 20.0)

        val expected = listOf(1.0, 2.0, 3.0)
        val actual = solver.solve(denseMatrix, b, eps)

        assertEqualsEps(expected, actual, eps)
    }

    @Test
    fun solveSymmetricProfile() {
        val symmetricProfileMatrix = SymmetricProfileMatrix(
            DenseMatrix(
            mutableListOf(
                mutableListOf(4.0, 1.0, 1.0),
                mutableListOf(1.0, 6.0, -1.0),
                mutableListOf(1.0, 2.0, 5.0),
            )
        )
        )
        val b = mutableListOf(9.0, 10.0, 20.0)

        val expected = listOf(1.0, 2.0, 3.0)
        val actual = solver.solve(symmetricProfileMatrix, b, eps)

        assertEqualsEps(expected, actual, eps)
    }
}
