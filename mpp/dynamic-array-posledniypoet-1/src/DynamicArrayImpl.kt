import kotlinx.atomicfu.*

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val realSize = atomic(0)

    override fun get(index: Int): E {
        require(index < size)
        while (true) {
            if (core.value.size > index) {
                return core.value.array[index].value?.myRef?.value ?: continue
            }
        }
    }

    override fun put(index: Int, element: E) {
        require(index < size)
        while (true) {
            val curCore = core.value;
            if (index >= curCore.size) {
                continue
            }
            val curContainer = curCore.array[index].value ?: continue
            val oldValue = curContainer.myRef.value ?: continue
            if (curContainer.myRef.compareAndSet(oldValue, element)) {
                if (curCore.array[index].value == curContainer) {
                    return
                }
            }
        }
    }

    override fun pushBack(element: E) {
        val pos = realSize.getAndIncrement()
        while (true) {
            val curCore = core.value
            if (pos >= curCore.size) {
                duplicate(curCore)
            } else {
                if (curCore.array[pos].compareAndSet(null, MyRef(atomic(element)))) {
                    break
                }
            }
        }
    }

    private fun duplicate(curCore: Core<E>) {
        val newCore = Core<E>(2 * curCore.size)
        if (curCore.next.compareAndSet(null, newCore)) {
            for (i in 0 until curCore.size) {
                while (true) {
                    val element = curCore.array[i].getAndSet(null)
                    if (element != null) {
                        newCore.array[i].compareAndSet(null, element)
                        break
                    }
                }
            }
            core.compareAndSet(curCore, newCore)
        }
    }

    override val size: Int
        get() {
            return realSize.value
        }
}

private class Core<E>(val size: Int) {
    val array = atomicArrayOfNulls<MyRef<E>>(size)
    val next: AtomicRef<Core<E>?> = atomic(null)
}

private class MyRef<T>(
    val myRef: AtomicRef<T?>
)

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME