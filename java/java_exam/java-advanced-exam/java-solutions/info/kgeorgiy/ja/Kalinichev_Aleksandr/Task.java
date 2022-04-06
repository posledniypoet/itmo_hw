package info.kgeorgiy.ja.Kalinichev_Aleksandr;

public class Task {
    private final String title;
    private final String description;

    public Task(final String title, final String description) {
        this.title = title;
        this.description = description;
    }

    public Task(final String task) {
        // :NOTE: - В заголовке может быть :
        final String[] splitted = task.split(":", 2);
        this.title = splitted[0].trim();
        this.description = splitted.length > 1 ? splitted[1].trim() : "";
    }

    public String getTitle() {
        return title;
    }

    public String getDescription() {
        return description;
    }
}