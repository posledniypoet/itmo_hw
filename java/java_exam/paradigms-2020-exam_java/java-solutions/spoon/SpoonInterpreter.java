package spoon;

import spoon.exception.InterpretException;
import spoon.exception.MemoryLimitException;
import spoon.exception.ProgramLengthLimitException;
import spoon.exception.UnknownCommandException;
import util.ConsoleSupplier;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class SpoonInterpreter {
    private static final List<String> COMMANDS = List.of("1", "000", "010", "011", "00100", "0011", "0010110", "001010");
    private final int memorySize;
    private final int maxProgramSize;
    private final Supplier<Integer> input;
    private final Consumer<Integer> output;

    public static void main(String[] args) throws MemoryLimitException, ProgramLengthLimitException {
        new SpoonInterpreter(new ConsoleSupplier(), (x) -> System.out.print((char) x.intValue()))
                .runProgram("0101111111110010001111111111010000001101100101001011111110010001111110" +
                "1000000110111001010111111100101000101011100101001011111111111001000110" +
                "0000000000000000001000000110110000010100000000000000000000000000000000" +
                "0000000101001011111111111001000111111101000000110110010100101111110010" +
                "0011111101000000110110010101110010100000000000000000000010100000000000" +
                "0000000000000000101001011111111111001000110000000000000000000100000011" +
                "011000001010");
    }

    public SpoonInterpreter() {
        this.input = new ConsoleSupplier();
        this.output = System.out::print;
        this.memorySize = 1024;
        this.maxProgramSize = 1024;
    }

    public SpoonInterpreter(final Supplier<Integer> input, final Consumer<Integer> output) {
        this.input = input;
        this.output = output;
        this.memorySize = 1024;
        this.maxProgramSize = 1024;
    }

    public SpoonInterpreter(final Supplier<Integer> input, final Consumer<Integer> output,
                            final int memorySize, final int maxProgramSize) {
        this.input = input;
        this.output = output;
        this.memorySize = memorySize;
        this.maxProgramSize = maxProgramSize;
    }

    private enum SpoonCommands {
        INC, DEC, MOVE_RIGHT, MOVE_LEFT, BEGIN, END, INPUT, OUTPUT
    }

    public void runProgram(String program) throws ProgramLengthLimitException, MemoryLimitException {
        List<SpoonCommands> commands = parse(program);
        if (commands.size() > maxProgramSize) {
            throw new ProgramLengthLimitException(maxProgramSize);
        }
        int pointer = 0;
        int[] memory =  new int[memorySize];
        Deque<Integer> loopMarks = new ArrayDeque<>();
        for (int commandIndex = 0; commandIndex < commands.size(); commandIndex++) {
            if (pointer > memorySize) {
                throw new MemoryLimitException(memorySize);
            }
            switch (commands.get(commandIndex)) {
                case INC:
                    memory[pointer] += 1;
                    break;
                case DEC:
                    memory[pointer] -= 1;
                    break;
                case MOVE_RIGHT:
                    pointer++;
                    break;
                case MOVE_LEFT:
                    pointer--;
                    break;
                case INPUT:
                    memory[pointer] = input.get();
                    break;
                case OUTPUT:
                    output.accept(memory[pointer]);
                    break;
                case BEGIN:
                    loopMarks.addLast(commandIndex);
                    break;
                case END:
                    if (!loopMarks.isEmpty()) {
                        if (memory[pointer] != 0) {
                            commandIndex = loopMarks.getLast();
                        } else {
                            loopMarks.removeLast();
                        }
                    }
                    break;
            }
        }
    }

    private static List<SpoonCommands> parse(String program) {
        int ind = 0;
        List<SpoonCommands> parsedCommands = new ArrayList<>();

        w: while (ind < program.length()) {
            while (ind < program.length() && Character.isWhitespace(program.charAt(ind))) {
                ind++;
            }
            if (ind == program.length()) {
                break;
            }
            for (String command: COMMANDS) {
                if (program.startsWith(command, ind)) {
                    ind += command.length();
                    parsedCommands.add(parseCommand(command));
                    continue w;
                }
            }
            throw new InterpretException(ind);
        }
        return parsedCommands;
    }

    private static SpoonCommands parseCommand(String str) {
        switch (str) {
            case "1":
                return SpoonCommands.INC;
            case "000":
                return SpoonCommands.DEC;
            case "010":
                return SpoonCommands.MOVE_RIGHT;
            case "011":
                return SpoonCommands.MOVE_LEFT;
            case "00100":
                return SpoonCommands.BEGIN;
            case "0011":
                return SpoonCommands.END;
            case "0010110":
                return SpoonCommands.INPUT;
            case "001010":
                return SpoonCommands.OUTPUT;
        }
        throw new UnknownCommandException(str);
    }
}
