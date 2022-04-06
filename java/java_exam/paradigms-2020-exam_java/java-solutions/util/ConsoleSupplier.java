package util;

import java.util.Scanner;
import java.util.function.Supplier;

public class ConsoleSupplier implements Supplier<Integer> {
    private final Scanner sc = new Scanner(System.in);
    public Integer get() {
        return sc.nextInt();
    }
}
