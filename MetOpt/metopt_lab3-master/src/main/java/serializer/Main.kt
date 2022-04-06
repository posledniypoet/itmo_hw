package serializer

import generator.generateB
import generator.generateDiagonallyDominantMatrix
import generator.generateHilbertMatrix
import generator.generateIncrementalVector
import generator.generateSymmetricDiagonallyDominantDenseMatrix
import matrix.DenseMatrix
import matrix.SymmetricProfileMatrix
import matrix.SymmetricSparseRowColumnMatrix

fun save(a: SymmetricSparseRowColumnMatrix, b: List<Double>, name: String) {
    try {
        val io = IO(outFile = name + "_sparse.txt")
        write(a, io)
        write(b, io)
        io.close()
    } catch (e: Exception) {
        e.printStackTrace()
    }
}

fun save(a: SymmetricProfileMatrix, b: List<Double>, name: String) {
    try {
        val io = IO(outFile = name + "_symmetric.txt")
        write((a), io)
        write(b, io)
        io.close()
    } catch (e: Exception) {
        e.printStackTrace()
    }
}

fun save(a: DenseMatrix, b: List<Double>, name: String) {
    try {
        val io = IO(outFile = name + "_dense_a.txt")
        write(a, io)
        write(b, io)
        io.close()
    } catch (e: Exception) {
        e.printStackTrace()
    }
}

fun generateDiagonallyDominant(nameArg: String) {
    val name = nameArg + "_diagonally_dominant"

    listOf(10, 100, 1000).forEach { n ->
        val e = generateIncrementalVector(n)
        (0..10).forEach { k ->
            (0 until 10).forEach { i ->
                val a = generateDiagonallyDominantMatrix(n, k)
                val b = generateB(a, e)
                val fileName = "${name}_${n}_${k}_${i}"
                save(a, b, fileName)
                println("Generated DiagonallyDominantMatrix n=$n k=$k\n")
            }
        }
    }
}

fun generateDiagonallyDominantAsSPM(nameArg: String) {
    val name = nameArg + "_diagonally_dominant"

    listOf(10, 100, 1000).forEach { n ->
        val e = generateIncrementalVector(n)
        (0..10).forEach { k ->
            (0 until 10).forEach { i ->
                val a = generateDiagonallyDominantMatrix(n, k)
                val b = generateB(a, e)
                val fileName = "${name}_${n}_${k}_${i}"
                save(SymmetricProfileMatrix(a), b, fileName)
                println("Generated DiagonallyDominantMatrix n=$n k=$k\n")
            }
        }
    }
}

fun generateHilbertAsSparse(nameArg: String) {
    val name = nameArg + "_hilbert"

    listOf(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000).forEach { n ->
        (0 until 10).forEach { i ->
            val e = generateIncrementalVector(n)
            val a = generateHilbertMatrix(n)
            val b = generateB(a, e)

            val fileName = "${name}_${n}_${i}"
            save(SymmetricSparseRowColumnMatrix(a), b, fileName)
            println("Generated HilbertMatrix n=$n\n")
        }
    }
}

fun generateHilbertAsSPM(nameArg: String) {
    val name = nameArg + "_hilbert"

    listOf(10, 20, 30, 40, 50, 100, 200, 300, 400, 500, 1000).forEach { n ->
        (0 until 10).forEach { i ->
            val e = generateIncrementalVector(n)
            val a = generateHilbertMatrix(n)
            val b = generateB(a, e)

            val fileName = "${name}_${n}_${i}"
            save(SymmetricProfileMatrix(a), b, fileName)
            println("Generated HilbertMatrix n=$n\n")
        }
    }
}

fun generateSymmetricDiagonallyDominant(nameArg: String, invert: Boolean) {
    val name = nameArg + "_symmetric_diagonally_dominant${if (invert) "_inv" else ""}"

    listOf(10, 100, 1000).forEach { n ->
        (0 until 10).forEach { i ->
            val e = generateIncrementalVector(n)
            val a = generateSymmetricDiagonallyDominantDenseMatrix(n, invert)
            val b = generateB(a, e)

            val fileName = "${name}_${n}_${i}"
            save(SymmetricSparseRowColumnMatrix(a), b, fileName)
            println("Generated HilbertMatrix n=$n\n")
        }
    }
}

fun main() {
    val path = "resources/test/ololo"

    generateDiagonallyDominantAsSPM(path)
    generateHilbertAsSPM(path)
    generateDiagonallyDominant(path)
    generateHilbertAsSparse(path)
    generateSymmetricDiagonallyDominant(path, true)
}
