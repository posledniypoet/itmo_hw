package generator

import matrix.DenseMatrix
import matrix.SymmetricProfileMatrix
import matrix.SymmetricSparseRowColumnMatrix
import serializer.IO
import serializer.read
import solver.ConjugateGradientInPlaceSolver
import solver.GaussPivotingInPlaceSolver
import solver.LUInPlaceSolver
import solver.abs
import solver.minus
import kotlin.time.seconds

val emptyDense = { DenseMatrix(mutableListOf(mutableListOf(0.0))) }
val eps = 1e-7

val conjugateGradientInPlaceSolver = ConjugateGradientInPlaceSolver()
val luInPlaceSolver = LUInPlaceSolver()
val gaussPivotingInPlaceSolver = GaussPivotingInPlaceSolver()


fun getInaccuracy(results: List<List<Double>>): Pair<Double, Double> {
    var abs = 0.0
    var rel = 0.0
    val ans = generateIncrementalVector(results[0].size)

    results.forEach { result ->
        val curAbs = (ans - result).abs()
        val curRel = curAbs / ans.abs()
        abs += curAbs
        rel += curRel
    }
    abs /= results.size
    rel /= results.size

    return abs to rel
}

fun getFullInaccuracy(results: List<List<Double>>, discrepancy: List<Double>): Pair<Pair<Double, Double>, Double> {
    var abs = 0.0
    var rel = 0.0
    var disc = 0.0
    val ans = generateIncrementalVector(results[0].size)

    results.forEachIndexed { i, result ->
        val curAbs = (ans - result).abs()
        val curRel = curAbs / ans.abs()
        abs += curAbs
        rel += curRel
        disc += curRel / discrepancy[i]
    }
    abs /= results.size
    rel /= results.size
    disc /= results.size

    return (abs to rel) to disc
}

fun printDDMResults(results: List<List<Double>>, n: Int, k: Int) {
    val res = getInaccuracy(results)
    // println("$n\t$k\t${res.first}\t${res.second}")
    println(String.format("%d\t%d\t%15.10e\t%15.10e", n, k, res.first, res.second))
}

fun printHilbertResults(results: List<List<Double>>, n: Int) {
    val res = getInaccuracy(results)
    // println("$n\t${res.first}\t${res.second}")
    println(String.format("%d\t%15.10e\t%15.10e", n, res.first, res.second))
}

fun printSDDResults(results: List<List<Double>>, n: Int, discrepancy: List<Double>) {
    val res = getFullInaccuracy(results, discrepancy)
    // println("$n\t${res.first.first}\t${res.first.second}\t${res.second}")
    println(String.format("%d\t%15.10e\t%15.10e\t%15.10e", n, res.first.first, res.first.second, res.second))
}

fun loadSparse(name: String): Pair<SymmetricSparseRowColumnMatrix, List<Double>> {
    val io = IO(inFile = name + "_sparse.txt")
    val matrix = SymmetricSparseRowColumnMatrix(emptyDense())
    read(matrix, io)
    val b = read(io)
    io.close()
    return matrix to b
}

fun loadSPM(name: String): Pair<SymmetricProfileMatrix, List<Double>> {
    val io = IO(inFile = name + "_symmetric.txt")
    val matrix = SymmetricProfileMatrix(emptyDense())
    read(matrix, io)
    val b = read(io)
    io.close()
    return matrix to b
}

fun loadDense(name: String): Pair<DenseMatrix, List<Double>> {
    val io = IO(inFile = name + "_dense_a.txt")
    val matrix = DenseMatrix(mutableListOf(mutableListOf()))
    read(matrix, io)
    val b = read(io)
    io.close()
    return matrix to b
}

fun loadSparseSolveByCG(name: String): Pair<List<Double>, Double> {
    val (matrix, b) = loadSparse(name)
    val xs = conjugateGradientInPlaceSolver.solve(matrix, b.toMutableList(), eps)
    val bs = generateB(matrix, xs)
    val discrepancy = (b - bs).abs() / b.abs()
    return xs to discrepancy
}

fun loadSPMSolveByLU(name: String): List<Double> {
    val (matrix, b) = loadSPM(name)
    return luInPlaceSolver.solve(matrix, b.toMutableList(), eps)
}

fun loadDenseSolveByGauss(name: String): List<Double> {
    val (matrix, b) = loadDense(name)
    return gaussPivotingInPlaceSolver.solve(matrix, b.toMutableList(), eps)
}

fun loadSolveSDDM(nameArg: String, invert: Boolean) {
    val name = nameArg + "_symmetric_diagonally_dominant${if (invert) "_inv" else ""}"

    listOf(10, 100, 1000).forEach { n ->
        val disc = mutableListOf<Double>()
        val results = (0 until 10).map { i ->
            val fileName = "${name}_${n}_${i}"
            val (xs, discrepancy) = loadSparseSolveByCG(fileName)
            disc.add(discrepancy)
            xs
        }
        printSDDResults(results, n, disc)
    }
}

fun loadSolveDDMByLU(nameArg: String) {
    val name = nameArg + "_diagonally_dominant"

    listOf(10, 100, 1000).forEach { n ->
        (0..10).forEach { k ->
            val results = (0 until 10).map { i ->
                val fileName = "${name}_${n}_${k}_${i}"
                loadSPMSolveByLU(fileName)
            }
            printDDMResults(results, n, k)
        }
    }
}

fun loadSolveDDMByGauss(nameArg: String) {
    val name = nameArg + "_diagonally_dominant"

    listOf(10, 100, 1000).forEach { n ->
        (0..10).forEach { k ->
            val results = (0 until 10).map { i ->
                val fileName = "${name}_${n}_${k}_${i}"
                loadDenseSolveByGauss(fileName)
            }
            printDDMResults(results, n, k)
        }
    }
}

fun loadSolveHilbertByLU(nameArg: String) {
    val name = nameArg + "_hilbert"
    listOf(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000).forEach { n ->
        val results = (0 until 10).map { i ->
            val fileName = "${name}_${n}_${i}"
            loadSPMSolveByLU(fileName)
        }
        printHilbertResults(results, n)
    }
}

fun loadSolveHilbertByGauss(nameArg: String) {
    val name = nameArg + "_hilbert"
    listOf(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000).forEach { n ->
        val results = (0 until 10).map { i ->
            val fileName = "${name}_${n}_${i}"
            loadDenseSolveByGauss(fileName)
        }
        printHilbertResults(results, n)
    }
}

fun loadSolveSparseHilbert(nameArg: String) {
    val name = nameArg + "_hilbert"
    listOf(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000).forEach { n ->
        val disc = mutableListOf<Double>()
        val results = (0 until 10).map { i ->
            val fileName = "${name}_${n}_${i}"
            val (xs, discrepancy) = loadSparseSolveByCG(fileName)
            disc.add(discrepancy)
            xs
        }
        printSDDResults(results, n, disc)
    }
}


fun main() {
    val path = "resources/test/ololo"

    loadSolveDDMByGauss(path)
    // loadSolveSparseHilbert(path)
    // loadSolveSDDM(path, true)
}
