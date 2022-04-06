package info.kgeorgiy.ja.kalinichev.arrayset;

import java.util.*;

public class ArraySet<T> extends AbstractSet<T> implements NavigableSet<T> {
    private final List<T> arrayList;
    private final Comparator<? super T> comparator;

    public ArraySet() {
        this.arrayList = Collections.emptyList();
        this.comparator = null;
    }

    private ArraySet(Comparator<? super T> comparator) {
        this.arrayList = Collections.emptyList();
        this.comparator = comparator;
    }


    public ArraySet(Collection<? extends T> collection) {
        this(collection, null);
    }


    public ArraySet(Collection<? extends T> collection, Comparator<? super T> comparator) {
        NavigableSet<T> newSet = new TreeSet<>(comparator);
        newSet.addAll(collection);
        this.arrayList = new ArrayList<>(newSet);
        this.comparator = comparator;
    }


    private ArraySet(List<T> arrayList, Comparator<? super T> comparator) {
        this.arrayList = arrayList;
        this.comparator = comparator;

        if (arrayList instanceof ReversedList) {
            ((ReversedList<T>) arrayList).reverse();
        }
    }

    @Override
    public T lower(T t) {
        return getElementInSet(t, -1, -1);
    }

    @Override
    public T floor(T t) {
        return getElementInSet(t, 0, -1);
    }

    @Override
    public T ceiling(T t) {
        return getElementInSet(t, 0, 0);
    }

    @Override
    public T higher(T t) {
        return getElementInSet(t, 1, 0);
    }

    @Override
    public Iterator<T> iterator() {
        return arrayList.iterator();
    }

    @Override
    public NavigableSet<T> descendingSet() {
        return new ArraySet<>(new ReversedList<>(arrayList), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<T> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<T> subSet(T fromElement, boolean fromInclusive, T toElement, boolean toInclusive) {
        if (comparator.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("left border less than right border");
        }
        int fromIndex = fromInclusive ? indexOf(fromElement, 0, 0) : indexOf(fromElement, 1, 0);
        int toIndex = toInclusive ? indexOf(toElement, 0, -1) : indexOf(toElement, -1, -1);
        if (fromIndex > toIndex || fromIndex == -1 || toIndex == -1) {
            return new ArraySet<>(comparator);
        }
        return new ArraySet<>(arrayList.subList(fromIndex, toIndex + 1), comparator);
    }

    @Override
    public NavigableSet<T> headSet(T toElement, boolean inclusive) {
        try {
            return (arrayList.isEmpty()) ? new ArraySet<>(comparator) : subSet(first(), true, toElement, inclusive);
        } catch(IllegalArgumentException e){

        }
        return new ArraySet<>(comparator);
    }

    @Override
    public NavigableSet<T> tailSet(T fromElement, boolean inclusive) {
        try {
            return (arrayList.isEmpty()) ? new ArraySet<>(comparator) : subSet(fromElement, inclusive, last(), true);
        } catch (IllegalArgumentException e){

        }
        return new ArraySet<>(comparator);
    }

    @Override
    public Comparator<? super T> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<T> subSet(T fromElement, T toElement) {
        if (comparator.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("left border less than right border");
        }
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public T first() {
        checkNotEmpty();
        return arrayList.get(0);
    }

    @Override
    public T last() {
        checkNotEmpty();
        return arrayList.get(size() - 1);
    }

    @Override
    public int size() {
        return arrayList.size();
    }


    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return Collections.binarySearch(arrayList, (T) Objects.requireNonNull(o), comparator) >= 0;
    }

    private boolean checkInd(int ind) {
        return 0 <= ind && ind <= arrayList.size() - 1;
    }


    private void checkNotEmpty() {
        if (arrayList.isEmpty()) {
            throw new NoSuchElementException();
        }
    }

    private int indexOf(T element, int includeFound, int includeNotFound) {
        int i = Collections.binarySearch(arrayList, Objects.requireNonNull(element), comparator);
        if (i < 0) {
            i = -(i + 1);
            return checkInd(i + includeNotFound) ? (i + includeNotFound) : -1;
        }
        return checkInd(i + includeFound) ? (i + includeFound) : -1;
    }

    private T getElementInSet(T element, int includeFound, int includeNotFound) {
        int i = indexOf(element, includeFound, includeNotFound);
        return checkInd(i) ? arrayList.get(i) : null;
    }

    @Override
    public T pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T pollLast() {
        throw new UnsupportedOperationException();
    }

    private static class ReversedList<E> extends AbstractList<E> implements RandomAccess {
        private final List<E> arrayList;
        private boolean isReversed;

        private ReversedList(List<E> data) {
            this.arrayList = data;
        }

        @Override
        public E get(int index) {
            return isReversed ? arrayList.get(size() - 1 - index) : arrayList.get(index);
        }

        @Override
        public int size() {
            return arrayList.size();
        }

        void reverse() {
            isReversed = !isReversed;
        }
    }
}