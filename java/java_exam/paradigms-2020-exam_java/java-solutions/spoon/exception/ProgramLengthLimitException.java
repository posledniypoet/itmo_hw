package spoon.exception;

public class ProgramLengthLimitException extends Exception {
    public ProgramLengthLimitException(int maxProgramLength) {
        super("Excess of limit (" + maxProgramLength + ")");
    }
}
