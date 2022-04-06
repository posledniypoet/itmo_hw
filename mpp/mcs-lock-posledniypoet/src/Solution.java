import java.util.concurrent.atomic.*;

public class Solution implements Lock<Solution.Node> {
    private final Environment env;
    private final AtomicReference<Node> tail;

    // todo: необходимые поля (final, используем AtomicReference)

    public Solution(Environment env) {
        this.env = env;
        this.tail = new AtomicReference<>(null);
    }

    @Override
    public Node lock() {
        Node my = new Node(); // сделали узел
        my.flag.getAndSet(true);
        Node prev = tail.getAndSet(my);
        if (prev != null) {
            prev.next.getAndSet(my);
            while (my.flag.get()) {
                env.park();
            }
        }
        return my; // вернули узел
    }

    @Override
    public void unlock(Node node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) {
                return;
            } else {
                while (node.next.get() == null) {
                }
            }
        }
        node.next.get().flag.set(false);
        env.unpark(node.next.get().thread);
    }

    static class Node {
        final Thread thread = Thread.currentThread(); // запоминаем поток, которые создал узел
        final AtomicReference<Boolean> flag = new AtomicReference<>(false);
        final AtomicReference<Node> next = new AtomicReference<>(null);
    }
}