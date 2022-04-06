package info.kgeorgiy.ja.Kalinichev_Aleksandr;
import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class Main {
    private static final Set<String> locales = Set.of("en", "ru");

    public static void main(final String[] args) {
        if (args == null || args.length != 1 || args[0] == null) {
            System.out.println("Usage: Main <language>");
            return;
        }
        console(args[0]);
    }

    public static void console(final String lang) {
        // :NOTE: * Ограничение локалей в коде
        if (!locales.contains(lang)) {
            // :NOTE: - String.join(", ", locales)
            System.out.printf("Available locales: %s%n", locales.stream().collect(Collectors.joining(", ")));
        }
        TodoList list = new TodoList();
        final ResourceBundle bundle = ResourceBundle.getBundle("TodoResourceBundle", Locale.forLanguageTag(lang));
        System.out.println(bundle.getString("helloMessage"));
        try (final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            // :NOTE: * Бесполезное оборачивание System.out
            try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out))) {
                while (true) {
                    writer.flush();
                    final String[] input = reader.readLine().trim().split("\\p{javaWhitespace}", 2);
                    final String command = input[0];
                    if (input.length == 2) {
                        final String args = input[1].trim();
                        switch (command.toLowerCase()) {
                            case "load": {
                                try {
                                    list = TodoList.load(args);
                                } catch (final IOException e) {
                                    System.out.printf("%s: %s%n", bundle.getString("loadError"), e.getMessage());
                                }
                                continue;
                            }
                            case "save": {
                                try {
                                    list.save(args);
                                } catch (final IOException e) {
                                    System.out.printf("%s: %s%n", bundle.getString("saveError"), e.getMessage());
                                }
                                continue;
                            }
                            case "add": {
                                list.add(args);
                                continue;
                            }
                            case "remove": {
                                if (!list.remove(args)) {
                                    // :NOTE: * Конкатенация строк в локалиованном контексте
                                    System.out.printf("%s: %s%n", bundle.getString("removeEmpty"), args);
                                }
                                continue;
                            }
                            case "complete": {
                                if (!list.complete(args)) {
                                    System.out.printf("%s: %s%n", bundle.getString("completeEmpty"), args);
                                }
                                continue;
                            }
                        }
                    }
                    final Collection<Task> tasks;
                    switch (command.toLowerCase()) {
                        case "all": {
                            tasks = list.all();
                            break;
                        }
                        case "remaining": {
                            tasks = list.remaining();
                            break;
                        }
                        case "done": {
                            tasks = list.done();
                            break;
                        }
                        case "exit":
                            return;
                        default: {
                            System.out.println(bundle.getString("help"));
                            continue;
                        }
                    }
                    TodoList.writeTasks(writer, tasks);
                    if (tasks.isEmpty()) {
                        writer.write(bundle.getString("noTasks") + System.lineSeparator());
                    }
                }
            } catch (final IOException e) {
                System.err.printf("%s: %s%n", bundle.getString("writingError"), e.getMessage());
            }
        } catch (final IOException e) {
            System.err.printf("%s: %s%n", bundle.getString("readingError"), e.getMessage());
        }
    }
}

