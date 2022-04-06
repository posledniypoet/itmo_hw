package linked_list_set;

import kotlinx.atomicfu.AtomicRef;

public class SetImpl implements Set {
    private static class Node {
        final AtomicRef<Node> next;
        int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }

        Node() {
            next = new AtomicRef<>(null);
        }
    }

    private class Window {
        Node cur, next;
    }

    private final Node head = new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null));

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(int x) {
        while (true) {
            Node cur = head;
            Node next = cur.next.getValue();
            if (next instanceof Removed) {
                next = ((Removed) next).next;
            }
            while (next.x < x) {
                if (next.next.getValue() instanceof Removed) {
                    if (!cur.next.compareAndSet(next, ((Removed) next.next.getValue()).next)) {
                        cur = head;
                        next = cur.next.getValue();
                        if (next instanceof Removed) {
                            next = ((Removed) next).next;
                        }
                    } else {
                        next = ((Removed) next.next.getValue()).next;
                    }
                } else {
                    cur = next;
                    next = cur.next.getValue();
                    if (next instanceof Removed) {
                        next = ((Removed) next).next;
                    }
                }
            }
            Node nextNext = next.next.getValue();
            if (nextNext instanceof Removed) {
                cur.next.compareAndSet(next, ((Removed) nextNext).next);
            } else {
                Window w = new Window();
                w.cur = cur;
                w.next = next;
                return w;
            }

        }
    }

    @Override
    public boolean add(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.x == x) {
                return false;
            }
            if (w.cur.next.compareAndSet(w.next, new Node(x, w.next))) return true;
        }
    }

    @Override
    public boolean remove(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.x != x) {
                return false;
            }
            Node next = w.next.next.getValue();
            if (next instanceof Removed) {
                next = ((Removed) next).next;
            }
            if (w.next.next.compareAndSet(next, new Removed(next))) {
                w.cur.next.compareAndSet(w.next, next);
                return true;
            }
        }
    }

    @Override
    public boolean contains(int x) {
        Window w = findWindow(x);
        return w.next.x == x;
    }

    private static class Removed extends Node {
        final Node next;

        Removed(Node next) {
            this.next = next;
        }
    }

}