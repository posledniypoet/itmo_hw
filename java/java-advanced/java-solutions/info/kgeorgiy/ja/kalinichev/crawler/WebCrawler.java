package info.kgeorgiy.ja.kalinichev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Phaser;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class WebCrawler implements Crawler {

    private final ThreadPool downloadersPool;
    private final ThreadPool extractorsPool;
    private final Downloader downloader;
    private final int perHost;
    private final Map<String, HostCounter> hostCounters;

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloadersPool = new ThreadPool(downloaders);
        this.extractorsPool = new ThreadPool(extractors);
        this.downloader = downloader;
        this.perHost = perHost;
        this.hostCounters = new ConcurrentHashMap<>();
    }

    @Override
    public Result download(String url, int depth) {
        final Map<String, IOException> errors = new ConcurrentHashMap<>();
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();

        Set<String> current = new HashSet<>();
        Set<String> next = ConcurrentHashMap.newKeySet();
        current.add(url);
        for (int i = 0; i < depth; i++) {
            Phaser phaser = new Phaser(1);
            downloaded.addAll(current);

            for (String s : current) {
                crawl(s, depth - i > 1, downloaded, errors, phaser, next);
            }
            phaser.arriveAndAwaitAdvance();

            current = next;
            next = ConcurrentHashMap.newKeySet();
        }

        downloaded.removeAll(errors.keySet());
        return new Result(new ArrayList<>(downloaded), errors);
    }

    private void crawl(final String url, final boolean needExtractLinks, final Set<String> downloaded,
                       final Map<String, IOException> errors, final Phaser phaser, final Set<String> next) {

        try {
            String host = URLUtils.getHost(url);
            HostCounter hostCounter = hostCounters.computeIfAbsent(host, h -> new HostCounter());

            Runnable downloadTask = () -> {
                try {
                    Document document = downloader.download(url);

                    if (needExtractLinks) {
                        Runnable extractTask = () -> {
                            try {
                                List<String> newLinks = document.extractLinks();
                                newLinks.forEach(newLink -> {
                                    if (!downloaded.contains(newLink)) {
                                        next.add(newLink);
                                    }
                                });
                            } catch (IOException e) {
                                errors.put(url, e);
                            } finally {
                                phaser.arrive();
                            }
                        };

                        phaser.register();
                        extractorsPool.addTask(extractTask);
                    }
                } catch (IOException e) {
                    errors.put(url, e);
                } finally {
                    phaser.arrive();
                    hostCounter.tryNext();
                }
            };

            phaser.register();
            hostCounter.add(downloadTask);
        } catch (MalformedURLException e) {
            errors.put(url, e);
        }
    }

    @Override
    public void close() {
        downloadersPool.close();
        extractorsPool.close();
    }

    class HostCounter {
        final LinkedList<Runnable> tasks;
        int executingCount;

        HostCounter() {
            tasks = new LinkedList<>();
            executingCount = 0;
        }

        private synchronized void add(Runnable task) {
            if (executingCount < perHost) {
                downloadersPool.addTask(task);
                executingCount++;
            } else {
                tasks.add(task);
            }
        }

        private synchronized void tryNext() {
            executingCount--;
            if (tasks.size() > 0) {
                add(tasks.pollFirst());
            }
        }
    }


    private static final int DEFAULT_DEPTH = 2;
    private static final int DEFAULT_DOWNLOADERS = 5;
    private static final int DEFAULT_EXTRACTORS = 5;
    private static final int DEFAULT_PER_HOST = 3;

    public static void main(String[] args) throws IOException {
        // :NOTE: args == null
        if (args.length == 0) {
            System.err.println("Usage: url [depth [downloaders [extractors [perHost]]]]");
            return;
        }

        String url = args[0];
        int depth = getArgument(args, 1, DEFAULT_DEPTH);
        int downloaders = getArgument(args, 2, DEFAULT_DOWNLOADERS);
        int extractors = getArgument(args, 3, DEFAULT_EXTRACTORS);
        int perHost = getArgument(args, 4, DEFAULT_PER_HOST);

        WebCrawler crawler = new WebCrawler(new CachingDownloader(), downloaders, extractors, perHost);
        Result result = crawler.download(url, depth);
        crawler.close();

        printSuccessful(result);
        printFailed(result);
    }

    private static void printSuccessful(final Result result) {
        System.out.println("Successful downloads: " + result.getDownloaded().size());
        result.getDownloaded().forEach(System.out::println);
    }

    private static void printFailed(final Result result) {
        System.out.println("Failed downloads: " + result.getErrors().size() + " page");
        result.getErrors().forEach((s, e) -> {
            System.out.println("URL: " + s);
            System.out.println("Error: " + e.getMessage());
        });
    }

    private static int getArgument(String[] args, int index, int defaultValue) {
        if (index < args.length) {
            int value = Integer.parseInt(args[index]);
            return value > 0 ? value : defaultValue;
        }

        return defaultValue;
    }
}

class ThreadPool {

    private final ExecutorService executorService;

    public ThreadPool(int threadCount) {
        executorService = Executors.newFixedThreadPool(threadCount);
    }

    public void addTask(Runnable task) {
        synchronized (executorService) {
            executorService.submit(task);
        }
    }

    // :NOTE: wait for termination
    public void close() {
        executorService.shutdown();
    }
}