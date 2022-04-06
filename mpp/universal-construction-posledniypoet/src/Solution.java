/**
 * @author :TODO: Kalinichev Alexander
 */
public class Solution implements AtomicCounter {
    private final Node head = new Node(0);
    private final ThreadLocal<Node> tail = ThreadLocal.withInitial(() -> head);

    public int getAndAdd(int x) {
        while (true) {
            final int old = tail.get().init;
            final Node newNode = new Node(old + x);
            tail.set(tail.get().next.decide(newNode));
            if (tail.get() == newNode) return old;
        }
    }

    // вам наверняка потребуется дополнительный класс
    private static class Node {
        final Consensus<Node> next = new Consensus<>();
        private final int init;

        Node(int init) {
            this.init = init;
        }
    }
}