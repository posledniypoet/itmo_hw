package tests

import org.junit.Assert.*

import matrix.Matrix

fun assertEqualsEps(expected: List<Double>, actual: List<Double>, eps: Double) {
    assertEquals(expected.size, actual.size)
    expected.indices.forEach { i ->
        assertEquals(expected[i], actual[i], eps)
    }
}

fun assertEqualsEps(expected: Matrix, actual: Matrix, eps: Double) {
    assertEquals(expected.size(), actual.size())
    (0 until expected.size()).forEach { i ->
        assertEqualsEps(
            (0 until expected.size()).map { j -> expected[i, j] },
            (0 until expected.size()).map { j -> actual[i, j] },
            eps
        )
    }
}
