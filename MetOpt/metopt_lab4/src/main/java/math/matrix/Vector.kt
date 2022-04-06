package math.matrix

import kotlin.math.sqrt

class Vector(array: List<Double>) {
    private val array = ArrayList<Double>()

    init {
        this.array.addAll(array)
    }

    fun data(): MutableList<Double> {
        return array
    }

    fun size(): Int {
        return array.size
    }

    operator fun unaryMinus(): Vector {
        return Vector(array.map { x -> -x })
    }

    operator fun times(b: Double): Vector {
        return Vector(array.map { x -> x * b })
    }

    operator fun plus(b: Vector): Vector {
        val arr = ArrayList<Double>()
        for (i in 0 until array.size) {
            arr.add(array[i] + b.array[i])
        }
        return Vector(arr)
    }

    fun scalar(b: Vector): Double {
        var ans = 0.0
        for (i in 0 until b.size()) {
            ans += array[i] * b.array[i]
        }
        return ans
    }

    operator fun minus(b: Vector): Vector {
        return this + -b
    }

    fun norm(): Double {
        return sqrt(this.scalar(this))
    }

    operator fun times(b: Vector): MutableList<MutableList<Double>> {
        return MutableList(size()) { i ->
            MutableList(b.size()) { j ->
                this.array[i] * b.array[j]
            }
        }
    }

    override fun toString(): String {
        return "$array"
    }

    fun equals(other: Vector, inaccuracy: Double): Boolean {
        if (array.size != other.size()) {
            return false
        }
        var d = 0.0
        for (i in 0 until array.size) {
            d += (array[i] - other.array[i]) * (array[i] - other.array[i])
        }

        return d <= inaccuracy * inaccuracy
    }

    fun copy(): Vector {
        return Vector(array.clone() as List<Double>)
    }
}
