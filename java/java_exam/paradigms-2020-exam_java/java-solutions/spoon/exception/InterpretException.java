package spoon.exception;

public class InterpretException extends RuntimeException {
    public InterpretException(int index) {
        super("Cannot interpret at " + index);
    }
}
