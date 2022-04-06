package com.example.metopt.math.minimization;

import java.util.function.Supplier;

import com.example.metopt.math.minimization.impl.Brent;
import com.example.metopt.math.minimization.impl.Dichotomy;
import com.example.metopt.math.minimization.impl.Fibonacci;
import com.example.metopt.math.minimization.impl.GoldenRatio;
import com.example.metopt.math.minimization.impl.Parabola;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public enum MethodEnum {

    DICHOTOMY(Dichotomy::new),

    GOLDEN_RATIO(GoldenRatio::new),

    PARABOLA(Parabola::new),

    FIBONACCI(Fibonacci::new),

    BRENT(Brent::new);

    private final Supplier<MinimizationMethod> method;

    public MinimizationMethod create() {
        return this.method.get();
    }

    MethodEnum(final Supplier<MinimizationMethod> method) {
        this.method = method;
    }
}
