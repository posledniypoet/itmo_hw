package math.matrix

interface Matrix {

    operator fun get(i: Int, j: Int): Double

    operator fun set(i: Int, j: Int, value: Double)

    fun size(): Int
}
