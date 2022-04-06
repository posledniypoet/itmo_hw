import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.*
import kotlin.math.max
import kotlin.math.min

class FCPriorityQueue<E : Comparable<E>> {
    private enum class OperationType {
        POLL, PEEK, ADD, DONE
    }

    private class Operation<E>(val type: OperationType, val value: E?)

    private val size = 4 * Runtime.getRuntime().availableProcessors()
    private val array = atomicArrayOfNulls<Operation<E>?>(size)
    private val q = PriorityQueue<E>()
    private val isLocked: AtomicBoolean = atomic(false)
    private val rnd: Random = Random()

    private fun addOp() {
        for (i in 0 until size) {
            val operation = array[i].value ?: continue
            if (operation.type == OperationType.POLL) {
                val res = q.poll()
                if (!array[i].compareAndSet(operation, Operation(OperationType.DONE, res)) && res != null) {
                    q.add(res)
                }
            } else if (operation.type == OperationType.ADD) {
                if (array[i].compareAndSet(operation, Operation(OperationType.DONE, null))) {
                    q.add(operation.value)
                }
            } else if (operation.type == OperationType.PEEK) {
                array[i].compareAndSet(operation, Operation(OperationType.DONE, q.peek()))
            } else {
                continue
            }
        }
    }


    private fun resolveOp(operation: Operation<E>): Pair<E?, Boolean> {
        while (true) {
            val ind = rnd.nextInt(size)
            for (i in max(ind - 2, 0) until min(ind + 2, size - 1)) {
                if (array[ind].compareAndSet(null, operation)) {
                    while (true) {
                        val op = array[ind].value as Operation<E>
                        if (op.type == OperationType.DONE && array[ind].compareAndSet(op, null)) {
                            return Pair(op.value, true)
                        } else {
                            if (array[ind].compareAndSet(operation, null)){
                                return Pair(null, false)
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        while (true) {
            if (isLocked.compareAndSet(false, true)) {
                try {
                    addOp()
                    return q.poll()
                } finally {
                    isLocked.compareAndSet(true, false)
                }
            } else {
                val helper = resolveOp(Operation(OperationType.POLL, null))
                if (helper.second) {
                    return helper.first
                }
            }
        }
    }


    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        while (true) {
            if (isLocked.compareAndSet(false, true)) {
                try {
                    addOp()
                    return q.peek()
                } finally {
                    isLocked.compareAndSet(true, false)
                }
            } else {

                val helper = resolveOp(Operation(OperationType.PEEK, null))
                if (helper.second) {
                    return helper.first
                }
            }
        }
    }

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        while (true) {
            if (isLocked.compareAndSet(false, true)) {
                try {
                    addOp()
                    q.add(element)
                    return
                } finally {
                    isLocked.compareAndSet(true, false)
                }
            } else {
                val operation = Operation(OperationType.ADD, element)
                val helper = resolveOp(operation)
                if (helper.second) {
                    return
                }
            }
        }
    }
}
