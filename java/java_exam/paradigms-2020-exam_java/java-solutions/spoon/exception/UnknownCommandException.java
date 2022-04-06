package spoon.exception;

public class UnknownCommandException extends RuntimeException {
    public UnknownCommandException(String command) {
        super("Unknown command: " + command);
    }
}
