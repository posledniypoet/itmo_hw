package info.kgeorgiy.ja.Kalinichev_Aleksandr;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TodoList {
    private final Map<String, Task> remaining;
    private final Map<String, Task> done;

    public TodoList() {
        this.remaining = new LinkedHashMap<>();
        this.done = new LinkedHashMap<>();
    }

    public TodoList(final Map<String, Task> remaining, final Map<String, Task> done) {
        this.remaining = remaining;
        this.done = done;
    }

    public List<Task> all() {
        return Stream.concat(remaining.values().stream(), done.values().stream()).collect(Collectors.toList());
    }

    public Collection<Task> remaining() {
        return remaining.values();
    }

    public Collection<Task> done() {
        return done.values();
    }

    public boolean remove(final String title) {
        Task task = remaining.remove(title);
        if (task != null) {
            return true;
        }
        task = done.remove(title);
        return task != null;
    }

    public boolean complete(final String title) {
        final Task task = remaining.remove(title);
        if (task == null) {
            return false;
        }
        done.put(title, task);
        return true;
    }

    public void add(final String taskAsStr) {
        final Task task = new Task(taskAsStr);
        remaining.put(task.getTitle(), task);
    }

    public void save(final String file) throws IOException {
        try (final BufferedWriter writer = Files.newBufferedWriter(Path.of(file), StandardCharsets.UTF_8)) {
            writer.write(String.format("Remaining:%n"));
            writeTasks(writer, remaining());
            writer.write(String.format("%nDone:%n"));
            writeTasks(writer, done());
        }
    }

    public static TodoList load(final String file) throws IOException {
        // :NOTE: - Не поддерживаются многострочные описания
        try (final BufferedReader reader = Files.newBufferedReader(Path.of(file), StandardCharsets.UTF_8)) {
            final Map<String, Task> remaining = new LinkedHashMap<>();
            final Map<String, Task> done = new LinkedHashMap<>();
            if (!reader.readLine().equals("Remaining:")) {
                throw new IllegalArgumentException("Wrong file format");
            }
            String line = reader.readLine();
            while (line != null && !line.isBlank()) {
                final Task task = new Task(line.trim());
                remaining.put(task.getTitle(), task);
                line = reader.readLine();
            }
            if (!reader.readLine().equals("Done:")) {
                throw new IllegalArgumentException("Wrong file format");
            }
            while (line != null && !line.isBlank()) {
                final Task task = new Task(line.trim());
                done.put(task.getTitle(), task);
                line = reader.readLine();
            }
            return new TodoList(remaining, done);
        }
    }

    public static void writeTasks(final Writer writer, final Collection<Task> tasks) throws IOException {
        for (final Task task : tasks) {
            writer.write(String.format("%s: %s%n", task.getTitle(), task.getDescription()));
        }
    }
}
