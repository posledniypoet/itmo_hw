package com.example.metopt.math.expression.parser;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.binary.*;

import java.util.function.BinaryOperator;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public enum BinaryOperation {

    ADD("+", Addition::new),

    SUB("-", Subtraction::new),

    MUL("*", Multiplication::new),

    DIV("/", Division::new),

    POW("^", Exponentiation::new);

    public final String token;
    public final BinaryOperator<Expression> constructor;

    BinaryOperation(String token, BinaryOperator<Expression> constructor) {
        this.token = token;
        this.constructor = constructor;
    }
}
