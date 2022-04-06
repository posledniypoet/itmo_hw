package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private final AtomicRef<Node> head;
    private final AtomicRef<Node> tail;

    public MSQueue() {
        Node dummy = new Node(0, null);
        this.head = new AtomicRef<Node>(dummy);
        this.tail = new AtomicRef<Node>(dummy);
    }

    @Override
    public void enqueue(int x) {
        Node newTail = new Node(x, null);
        while (true) {
            Node curTail = tail.getValue();
            if (curTail.next.compareAndSet(null, newTail)) {
                tail.compareAndSet(curTail, newTail);
                return;
            } else {
                tail.compareAndSet(curTail, curTail.next.getValue());
            }

        }
    }

    @Override
    public int dequeue() {
        while (true) {
            Node curHead = head.getValue();
            Node next = curHead.next.getValue();
            if (next == null) {
                return Integer.MIN_VALUE;
            }
            if (head.compareAndSet(curHead, next)) {
                return next.x;
            }
        }
    }

    @Override
    public int peek() {
        while (true) {
            Node curHead = head.getValue();
            Node next = curHead.next.getValue();
            if (next == null) {
                return Integer.MIN_VALUE;
            }
            return next.x;
        }
    }

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }
}