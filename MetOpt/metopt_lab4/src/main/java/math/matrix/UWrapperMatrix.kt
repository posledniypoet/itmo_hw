package math.matrix

class UWrapperMatrix(
    val matrix: Matrix
) : Matrix {
    override fun get(i: Int, j: Int): Double {
        return if (i > j) {
            0.0
        } else {
            matrix[i, j]
        }
    }

    override fun set(i: Int, j: Int, value: Double) {
        throw UnsupportedOperationException()
    }

    override fun size(): Int =
        matrix.size()
}
