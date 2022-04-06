package dijkstra

import java.util.*
import kotlin.random.Random
import java.util.concurrent.Phaser
import java.util.concurrent.atomic.AtomicInteger
import kotlin.Comparator
import kotlin.concurrent.thread


private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0

    // Create a priority (by distance) queue and add the start node into it
    val q = MyPriorityQueue(workers, NODE_DISTANCE_COMPARATOR) // TODO replace me with a multi-queue based PQ!
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger()
    activeNodes.incrementAndGet()
    repeat(workers) {
        thread {
            while (true) {
                val cur: Node = q.poll() ?: if (activeNodes.compareAndSet(0, 0)) break else continue
                for (e in cur.outgoingEdges) {
                    while (true) {
                        val oldDistance = e.to.distance
                        val newDistance = cur.distance + e.weight

                        if (oldDistance > newDistance) {
                            if (e.to.casDistance(oldDistance, newDistance)) {
                                q.add(e.to)
                                activeNodes.incrementAndGet()
                                break
                            } else continue
                        }
                        break
                    }
                }
                activeNodes.decrementAndGet()

            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class MyPriorityQueue(workers: Int, nodeDistanceComparator: Comparator<Node>) {
    val random = Random(0)
    val numOfQueues = 2 * workers
    val queues: MutableList<PriorityQueue<Node>> =
        Collections.nCopies(numOfQueues, PriorityQueue(nodeDistanceComparator))

    fun add(element: Node) {
        val index = random.nextInt(numOfQueues)
        val chosenQueue = queues[index]
        synchronized(chosenQueue) {
            chosenQueue.add(element)
        }
    }

    fun poll(): Node? {
        var i = random.nextInt(numOfQueues)
        var j = random.nextInt(numOfQueues)
        while (i == j) {
            i = random.nextInt(numOfQueues)
            j = random.nextInt(numOfQueues)
        }

        synchronized(queues[i]) {
            synchronized(queues[j]) {
                if (queues[i].peek() == null)
                    return queues[j].peek()
                if (queues[j].peek() == null)
                    return queues[i].peek()
                return if (queues[i].peek().distance < queues[j].peek().distance) queues[i].poll() else queues[j].poll()
            }
        }
    }


}
