package info.kgeorgiy.ja.Kalinichev_Aleksandr;

import java.util.ListResourceBundle;

// :NOTE: * Использоваие ListResourceBundle для локализации строк
public class TodoResourceBundle_en extends ListResourceBundle {
    // :NOTE: - Не константа
    private static Object[][] content = {
            {"removeEmpty", "No task with title"},
            {"completeEmpty", "No remaining task with title"},
            {"help", "Command: add/remove/complete <title>, all/remaining/done, save/load <file>, exit"},
            {"writingError", "Writing error occurred"},
            {"readingError", "Reading error occurred"},
            {"loadError", "Loading error occurred"},
            {"saveError", "Saving error occurred"},
            {"noTasks", "No tasks"},
            {"helloMessage", "Task have title and description separated by ':'. Type help to help"}
    };

    @Override
    protected Object[][] getContents() {
        return content;
    }
}
