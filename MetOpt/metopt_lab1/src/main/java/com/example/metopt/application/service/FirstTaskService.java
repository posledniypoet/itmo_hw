package com.example.metopt.application.service;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.parser.ExpressionParser;
import com.example.metopt.math.minimization.MethodEnum;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import org.springframework.stereotype.Service;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 28.02.2021
 */

@Service
public class FirstTaskService {

    public Expression parseFormula(String formula) {
        return ExpressionParser.parse(formula);
    }

    public MinimizationResult findMin(Expression expr, double l, double r, double inaccuracy, MethodEnum method) {
        return method.create().minimize(expr, l, r, inaccuracy);
    }
}
