package linear;

import linear.impl.*;

import java.util.function.Supplier;

public enum LinearMethodEnum {

    DICHOTOMY(Dichotomy::new),

    GOLDEN_RATIO(GoldenRatio::new),

    PARABOLA(Parabola::new),

    FIBONACCI(Fibonacci::new),

    BRENT(Brent::new);

    private final Supplier<MinimizationMethod> method;

    public MinimizationMethod create() {
        return this.method.get();
    }

    LinearMethodEnum(final Supplier<MinimizationMethod> method) {
        this.method = method;
    }
}
