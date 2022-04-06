package info.kgeorgiy.ja.Kalinichev_Aleksandr;

import java.util.ListResourceBundle;

public class TodoResourceBundle_ru extends ListResourceBundle {
    private static Object[][] content = {
            {"removeEmpty", "Нет задач с названием"},
            {"completeEmpty", "Нет предстоящих задач с названием"},
            {"help", "Команды: add/remove/complete <название>, all/remaining/done, save/load <файл>, exit"},
            {"writingError", "Ошибка записи"},
            {"readingError", "Ошибка чтения"},
            {"loadError", "Ошибка загрузки"},
            {"saveError", "Ошибка сохранения"},
            {"noTasks", "Нет задач"},
            {"helloMessage", "Задача имеет название и описание разделённые двоеточием. Наберите help для списка команд"}
    };

    @Override
    protected Object[][] getContents() {
        return content;
    }
}
