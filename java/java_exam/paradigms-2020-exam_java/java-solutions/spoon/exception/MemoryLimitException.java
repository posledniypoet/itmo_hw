package spoon.exception;

public class MemoryLimitException extends Exception {
    public MemoryLimitException(int maxMemorySize) {
        super("Excess of limit (" + maxMemorySize + ")");
    }
}
