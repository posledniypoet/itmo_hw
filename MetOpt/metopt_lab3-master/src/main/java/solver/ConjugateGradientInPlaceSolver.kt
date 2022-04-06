package solver

import matrix.SymmetricSparseRowColumnMatrix

class ConjugateGradientInPlaceSolver : Solver<SymmetricSparseRowColumnMatrix> {

    override fun solve(matrix: SymmetricSparseRowColumnMatrix, b: MutableList<Double>, eps: Double): List<Double> {
        var prevX = MutableList(matrix.size()) { 0.0 }
        prevX[0] = 1.0
        var prevR = b - (matrix * prevX)
        var prevZ = prevR.map { it }

        repeat(10_000) {
            val mz = matrix * prevZ
            val alpha = (prevR * prevR) / (mz * prevZ)
            val curX = (prevZ * alpha) + prevX
            val curR = prevR - (mz * alpha)
            val beta = (curR * curR) / (prevR * prevR)
            val curZ = (prevZ * beta) + curR
            if ((curR * curR) < eps * eps * (b * b)) {
                return curX
            }
            prevX = curX.toMutableList()
            prevR = curR
            prevZ = curZ
        }

        return prevX
    }
}
