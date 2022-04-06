package com.example.metopt.stats;

import com.example.metopt.api.dto.DrawType;
import com.example.metopt.api.dto.IterationWrapper;
import com.example.metopt.api.dto.MinimizationResultDto;
import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.parser.ExpressionParser;
import com.example.metopt.math.minimization.MethodEnum;
import com.example.metopt.math.minimization.entity.MinimizationResult;

public class StatsMain {
    public static void main(String[] args) {
        Expression expr = ExpressionParser.parse("-5*x^5+4*x^4-12*x^3+11*x^2-2*x+1");
        double left = -0.5, right = 0.5;
        System.out.print("eps\tlog(eps)\t");
        for (MethodEnum method : MethodEnum.values()) {
            System.out.print(method + "\t");
        }
        System.out.println();

        for (double eps = 0.01; Math.log10(eps) > -17; eps /= Math.sqrt(10)) {
            System.out.print(eps + "\t" + -Math.log10(eps) + "\t");
            for (MethodEnum method : MethodEnum.values()) {
                MinimizationResult result = method.create().minimize(expr, left, right, eps);
                System.out.print(result.calcCount + "\t");
            }
            System.out.println();
        }
    }
}
