package com.example.metopt.math.expression.exception;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public class UndefinedVariableException extends ExpressionException {

    public UndefinedVariableException(String message) {
        super(message);
    }
}
