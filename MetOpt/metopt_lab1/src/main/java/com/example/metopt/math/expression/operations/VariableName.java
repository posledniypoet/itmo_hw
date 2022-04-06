package com.example.metopt.math.expression.operations;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */

public enum VariableName {
    X("x"),

    Y("y");

    public final String token;

    VariableName(String token) {
        this.token = token;
    }
}
