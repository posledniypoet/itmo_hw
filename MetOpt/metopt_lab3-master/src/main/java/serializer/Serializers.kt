package serializer

import matrix.DenseMatrix
import matrix.SymmetricProfileMatrix
import matrix.SymmetricSparseRowColumnMatrix

fun write(matrix: SymmetricProfileMatrix, io: IO) {
    io.writeln(matrix.diagonal.joinToString(" "))
    io.writeln(matrix.columnUpperProfile.joinToString(" "))
    io.writeln(matrix.rowLowerProfile.joinToString(" "))
    io.writeln(matrix.indices.joinToString(" "))
}

fun read(matrix: SymmetricProfileMatrix, io: IO) {
    matrix.diagonal.clear()
    matrix.columnUpperProfile.clear()
    matrix.rowLowerProfile.clear()
    matrix.indices.clear()

    matrix.diagonal.addAll(io.readDoubles())
    matrix.columnUpperProfile.addAll(io.readDoubles())
    matrix.rowLowerProfile.addAll(io.readDoubles())
    matrix.indices.addAll(io.readInts())
}

fun write(matrix: SymmetricSparseRowColumnMatrix, io: IO) {
    io.writeln(matrix.diagonal.joinToString(" "))
    io.writeln(matrix.rowLowerProfile.joinToString(" "))
    io.writeln(matrix.indexFirst.joinToString(" "))
    io.writeln(matrix.indexNonZero.joinToString(" "))
}

fun read(matrix: SymmetricSparseRowColumnMatrix, io: IO) {
    matrix.diagonal.clear()
    matrix.rowLowerProfile.clear()
    matrix.indexFirst.clear()
    matrix.indexNonZero.clear()

    matrix.diagonal.addAll(io.readDoubles())
    matrix.rowLowerProfile.addAll(io.readDoubles())
    matrix.indexFirst.addAll(io.readInts())
    matrix.indexNonZero.addAll(io.readInts())
}

fun write(matrix: DenseMatrix, io: IO) {
    io.writeln(matrix.size())
    io.writeln(
        matrix.matrix.joinToString(System.lineSeparator()) { row ->
            row.joinToString(" ")
        }
    )
}

fun read(matrix: DenseMatrix, io: IO) {
    matrix.matrix.clear()

    val (n) = io.readInts()
    matrix.matrix.addAll(
        MutableList(n) {
            io.readDoubles().toMutableList()
        }
    )
}


fun write(list: List<Double>, io: IO) {
    io.writeln(list.joinToString(" "))
}

fun read(io: IO): List<Double> {
    return io.readDoubles()
}
