package info.kgeorgiy.ja.kalinichev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class IterativeParallelism implements ListIP {
    private ParallelMapper parallelMapper = null;

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    public IterativeParallelism() {
    }


    private void joinThreads(List<Thread> threads) throws InterruptedException {
        for (Thread thread : threads) {
            thread.join();
        }
    }


    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.map(Object::toString).collect(Collectors.joining()),
                stream -> stream.collect(Collectors.joining()));
    }

    @Override
    public <T> List <T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.filter(predicate).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList()));
    }


    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.map(f).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList()));
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.max(comparator).orElseThrow(),
                stream -> stream.max(comparator).orElseThrow());
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, Collections.reverseOrder(comparator));
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(value -> value));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return myProcess(threads, values,
                stream -> stream.anyMatch(predicate),
                stream -> stream.anyMatch(value -> value));
    }

    public <T, R> R myProcess(int threads, final List<? extends T> elements,
                              final Function<Stream<? extends T>, R> mapper,
                              final Function<? super Stream<R>, R> collector) throws InterruptedException {
        if (threads <= 0) {
            throw new IllegalArgumentException("Number of threads must be > 0");
        }
        threads = Math.max(1, Math.min(threads, elements.size()));
        List<Stream<? extends T>> groups = new ArrayList<>();
        List<R> result = new ArrayList<>(Collections.nCopies(threads, null));
        int size = elements.size() / threads;
        int rest = elements.size() % threads;
        int index = 0;
        while (index < elements.size()) {
            int tempSize = size;
            if (rest > 0) {
                tempSize++;
                rest--;
            }

            int lastElementIndex = Math.min(elements.size(), index + tempSize);
            if (index != lastElementIndex) {
                groups.add(elements.subList(index, lastElementIndex).stream());
            }
            index += tempSize;
        }
        if (parallelMapper != null) {
            result = parallelMapper.map(mapper, groups);
        } else {
            List<Thread> workers = new ArrayList<>();
            for (int i = 0; i < threads; i++) {
                final int pos = i;
                List<R> finalResult = result;
                Thread worker = new Thread(() -> finalResult.set(pos, mapper.apply(groups.get(pos))));
                workers.add(worker);
                worker.start();
            }
            joinThreads(workers);
        }

        return collector.apply(result.stream());

    }
}
