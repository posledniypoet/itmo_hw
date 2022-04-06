package matrix

import util.assertThrow

class SymmetricSparseRowColumnMatrix : Matrix {
    val diagonal: MutableList<Double> = mutableListOf()
    val rowLowerProfile: MutableList<Double> = mutableListOf()
    val indexFirst: MutableList<Int> = mutableListOf()
    val indexNonZero: MutableList<Int> = mutableListOf()

    constructor(n: Int) {
        diagonal.addAll(MutableList(n) { 0.0 })
        indexFirst.add(0)
    }

    constructor(matrix: Matrix) {
        indexFirst.add(0)
        for (i in 0 until matrix.size()) {
            var cnt = 0
            diagonal.add(matrix[i, i])
            for (j in 0 until i) {
                if (matrix[i, j] != 0.0) {
                    ++cnt
                    rowLowerProfile.add(matrix[i, j])
                    indexNonZero.add(j)
                }
            }
            indexFirst.add(cnt + indexFirst[i])
        }
    }

    override fun get(i: Int, j: Int): Double {
        var x = j
        var y = i
        if (x == y) {
            return diagonal[x]
        }
        if (x < y) {
            x = y.also { y = x }
        }
        (indexFirst[x] until indexFirst[x + 1]).map { index ->
            if (indexNonZero[index] == y) {
                return rowLowerProfile[index]
            }
        }
        return 0.0
    }

    override fun set(i: Int, j: Int, value: Double) {
        var x = j
        var y = i
        if (x == y) {
            diagonal[x] = value
            return
        }
        if (x < y) {
            x = y.also { y = x }
        }

        (indexFirst[x] until indexFirst[x + 1]).map { index ->
            if (indexNonZero[index] == y) {
                rowLowerProfile[index] = value
                return
            }
        }
    }

    override fun size(): Int =
        diagonal.size

    operator fun times(v: List<Double>): List<Double> {
        assertThrow(v.size == size(), "Incompatible sizes")
        var pos = 0
        val ans = MutableList(v.size) { 0.0 }
        v.indices.map { i ->
            val cnt = indexFirst[i+1] - indexFirst[i]
            (pos until cnt + pos).map { j ->
                val col = indexNonZero[j]
                ans[col] += rowLowerProfile[j] * v[i]
                ans[i] += rowLowerProfile[j] * v[col]
            }
            ans[i] += diagonal[i] * v[i]
            pos += cnt
        }
        return ans
    }

    fun insertLine(line: List<Double>) {
        var cnt = 0
        line.indices.map { j ->
            if (line[j] != 0.0) {
                ++cnt
                rowLowerProfile.add(line[j])
                indexNonZero.add(j)
            }
        }
        indexFirst.add(cnt + indexFirst[line.size])
    }
}
