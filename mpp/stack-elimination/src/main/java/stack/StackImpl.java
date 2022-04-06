package stack;


import kotlinx.atomicfu.AtomicArray;
import kotlinx.atomicfu.AtomicRef;


public class StackImpl implements Stack {
    private static final int ARRAY_SIZE = 8;
    private static final int WAIT_TIME = 4;
    private static final int OPERATION_TRIES = 2;

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    // head pointer
    private AtomicRef<Node> head = new AtomicRef<>(null);
    private AtomicArray<Integer> array = new AtomicArray(ARRAY_SIZE);

    @Override
    public void push(int x) {
        final int pos = (int) (Math.random() * (ARRAY_SIZE - 1));
        int left_flag = 0;
        if (pos + OPERATION_TRIES > ARRAY_SIZE - 1)
            left_flag = 1;
        if (left_flag == 0) {
            for (int i = pos; i < pos + OPERATION_TRIES; i++) {
                final Integer link = x;
                if (array.get(i).compareAndSet(null, link)) {
                    for (int j = 0; j < WAIT_TIME; j++) {
                        if (array.get(i).compareAndSet(null, null) || array.get(i).getValue() != link) {
                            return;
                        }
                    }
                    if (!array.get(i).compareAndSet(link, null)) {
                        return;
                    }
                }
            }
        } else {
            for (int i = pos; i > pos - OPERATION_TRIES; i--) {
                final Integer link = x;
                if (array.get(i).compareAndSet(null, link)) {
                    for (int j = 0; j < WAIT_TIME; j++) {
                        if (array.get(i).compareAndSet(null, null) || array.get(i).getValue() != link) {
                            return;
                        }
                    }
                    if (!array.get(i).compareAndSet(link, null)) {
                        return;
                    }
                }
            }

        }
        while (true) {
            Node curHead = head.getValue();
            Node newHead = new Node(x, curHead);
            if (head.compareAndSet(curHead, newHead)) {
                return;
            }
        }
    }

    @Override
    public int pop() {
        final int pos = (int) (Math.random() * (ARRAY_SIZE - 1));
        int left_flag = 0;
        if (pos + OPERATION_TRIES > ARRAY_SIZE - 1)
            left_flag = 1;
        if (left_flag == 0) {
            for (int i = pos; i < pos + OPERATION_TRIES; i++) {
                final Integer link = array.get(i).getValue();
                if (array.get(i).compareAndSet(link, null) && link != null) {
                    return link;
                }
            }
        } else {
            for (int i = pos; i > pos - OPERATION_TRIES; i--) {
                final Integer link = array.get(i).getValue();
                if (array.get(i).compareAndSet(link, null) && link != null) {
                    return link;
                }
            }
        }
        while (true) {
            Node curHead = head.getValue();
            if (curHead == null) return Integer.MIN_VALUE;
            if (head.compareAndSet(curHead, curHead.next.getValue())) {
                return curHead.x;
            }
        }
    }
}
