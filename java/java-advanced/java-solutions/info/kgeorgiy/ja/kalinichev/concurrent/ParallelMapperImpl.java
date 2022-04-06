package info.kgeorgiy.ja.kalinichev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {

    private final List<Thread> threads;
    private final Queue<Runnable> tasks;


    public ParallelMapperImpl(int threads) {
        checkNumberOfThreads(threads);
        this.threads = new ArrayList<>(threads);
        this.tasks = new LinkedList<>();
        for (int i = 0; i < threads; i++) {
            Thread thread = new Thread(() -> {
                try {
                    while (!Thread.currentThread().isInterrupted()) {
                        solveTask();
                    }
                } catch (InterruptedException ignored) {
                    Thread.currentThread().interrupt();
                }
            });
            thread.start();
            this.threads.add(thread);
        }
    }


    private void solveTask() throws InterruptedException {
        Runnable task;
        synchronized (tasks) {
            while (tasks.isEmpty()) {
                tasks.wait();
            }
            task = tasks.poll();
        }

        task.run();
    }

    public static void checkNumberOfThreads(int threads) {
        if (threads < 1) {
            throw new IllegalArgumentException("Number of threads must be positive");
        }
    }

    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        int elementsSize = args.size();
        List<R> result = new ArrayList<>(Collections.nCopies(elementsSize, null));

        Counter counter = new Counter(elementsSize);
        for (int index = 0; index < elementsSize; index++) {
            final int elementIndex = index;
            Runnable runnable = () -> {
                result.set(elementIndex, f.apply(args.get(elementIndex)));
                synchronized (counter) {
                    counter.inc();
                }
            };
            synchronized (tasks) {
                tasks.add(runnable);
                tasks.notify();
            }
        }

        synchronized (counter) {
            while (!counter.isReady()) {
                counter.wait();
            }
        }

        return result;
    }

    @Override
    public void close() {
        threads.forEach(Thread::interrupt);

        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private final class Counter {
        private int done;
        private int tasksNumber;
        private boolean isReady = false;

        Counter(int tasksNumber) {
            this.done = 0;
            this.tasksNumber = tasksNumber;
        }

        void inc() {
            if (++done == tasksNumber) {
                isReady = true;
                this.notifyAll();
            }
        }

        private boolean isReady() {
            return isReady;
        }
    }
}