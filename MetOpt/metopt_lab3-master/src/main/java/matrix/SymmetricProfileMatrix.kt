package matrix

class SymmetricProfileMatrix(
    matrix: Matrix
) : Matrix {
    val diagonal: MutableList<Double> = mutableListOf()
    val columnUpperProfile: MutableList<Double> = mutableListOf()
    val rowLowerProfile: MutableList<Double> = mutableListOf()
    val indices: MutableList<Int> = mutableListOf()

    init {
        indices.add(0)
        (0 until matrix.size()).map { i ->
            diagonal.add(matrix[i, i])
            var startJ = 0
            while (startJ < i && matrix[i, startJ] == 0.0) {
                ++startJ
            }

            indices.add(i - startJ + indices[i])
            (startJ until i).map { j ->
                rowLowerProfile.add(matrix[i, j])
                columnUpperProfile.add(matrix[j, i])
            }
        }
    }

    override fun get(i: Int, j: Int): Double {
        if (i == j) {
            return diagonal[i]
        }
        return if (i > j) {
            val shift: Int = getShift(i)
            if (j < shift) 0.0 else rowLowerProfile[getProfileIndex(shift, i, j)]
        } else {
            val shift: Int = getShift(j)
            if (i < shift) 0.0 else columnUpperProfile[getProfileIndex(shift, j, i)]
        }
    }

    override fun set(i: Int, j: Int, value: Double) {
        if (j == i) {
            diagonal[j] = value
            return
        }
        if (j < i) {
            val shift = getShift(i)
            if (j < shift && value != 0.0) {
                throw IndexOutOfBoundsException()
            }
            rowLowerProfile[getProfileIndex(shift, i, j)] = value
        } else {
            val shift = getShift(j)
            if (i < shift && value != 0.0) {
                throw IndexOutOfBoundsException()
            }
            columnUpperProfile[getProfileIndex(shift, j, i)] = value
        }
    }

    override fun size(): Int =
        diagonal.size

    fun getShift(a: Int): Int =
        a - (indices[a + 1] - indices[a])

    fun getProfileIndex(shift: Int, a: Int, b: Int): Int =
        indices[a] + b - shift
}
