package com.example.metopt.math.expression.parser;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.unary.Cosine;
import com.example.metopt.math.expression.operations.unary.Minus;
import com.example.metopt.math.expression.operations.unary.Sines;

import java.util.function.UnaryOperator;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public enum UnaryOperation {

    MINUS("-", Minus::new),

    SINES("sin", Sines::new),

    COSINE("cos", Cosine::new);

    public final String token;
    public final UnaryOperator<Expression> constructor;

    UnaryOperation(String token, UnaryOperator<Expression> constructor) {
        this.token = token;
        this.constructor = constructor;
    }
}
