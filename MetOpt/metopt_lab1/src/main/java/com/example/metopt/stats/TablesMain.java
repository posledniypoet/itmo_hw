package com.example.metopt.stats;

import com.example.metopt.application.service.FirstTaskService;
import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.parser.ExpressionParser;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.MethodEnum;
import com.example.metopt.math.minimization.entity.MinimizationResult;
import com.example.metopt.math.minimization.entity.ParabolaIterationInfo;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;

public class TablesMain {
    final static FirstTaskService service = new FirstTaskService();
    static Expression expr = ExpressionParser.parse("-5*x^5+4*x^4-12*x^3+11*x^2-2*x+1");
    static double eps = 1e-6;

    public static void main(String[] args) {
        writeMethods();
    }

    private static void writeMethods() {
        writeFirstType(MethodEnum.DICHOTOMY);
        System.out.println("\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n");
        writeFirstType(MethodEnum.FIBONACCI);
        System.out.println("\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n");
        writeFirstType(MethodEnum.GOLDEN_RATIO);
        System.out.println("\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n");
        writeParabols();
        System.out.println("\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n" +
                "% -----------------------------------------------------------------------------------------------------------------------\n");
        writeBrent();
    }

    private static void writeFirstType(MethodEnum method) {
        MinimizationResult result = service.findMin(
                expr,
                -0.5,
                0.5,
                eps,
                method);
//        System.out.println(method);
        System.out.println("\\begin{table}[h]\n" +
                "\\center\n" +
                "\\begin{tabular}{cccccccc}\n" +
                "\\toprule");
        System.out.println("left & right & $x_l$ & $y_l$ & $x_r$ & $y_r$ & min & attitude \\\\");
        System.out.println("\\midrule");
        for (int i = 0; i < result.segments.size(); i++) {
            SegmentIterationInfo v = result.segments.get(i);
            System.out.println(String.format("%.6f", v.getL()) + " & "
                    + String.format("%.6f", v.getR()) + " & "
                    + pointToString(v.getX1Point()) + " & "
                    + pointToString(v.getX2Point()) + " & "
                    + String.format("%.10f", v.getCurrentMin().y));
            if (i > 0) {
                SegmentIterationInfo prev = result.segments.get(i - 1);
                System.out.println(" & " + String.format("%.6f", (v.getR() - v.getL()) / (prev.getR() - prev.getL())) + " \\\\");
            } else {
                System.out.println(" & " + String.format("%.6f", 1.) + " \\\\");
            }
        }
        System.out.println("\\bottomrule\n" +
                "\\end{tabular}\n" +
                "\\caption{" + method + "}\n" +
                "\\end{table}");
    }

    private static void writeParabols() {
        MinimizationResult result = service.findMin(
                expr,
                -0.5,
                0.5,
                eps,
                MethodEnum.PARABOLA);
//        System.out.println(MethodEnum.PARABOLA);
        System.out.println("\\begin{table}[h]\n" +
                "\\center\n" +
                "\\begin{tabular}{cccccccc}\n" +
                "\\toprule");
        System.out.println("left & right & medium & min & attitude\\\\");
        System.out.println("\\midrule");
        for (int i = 0; i < result.parabolas.size(); i++) {
            ParabolaIterationInfo v = result.parabolas.get(i);
            System.out.println(
                    String.format("%.6f", v.getL()) + " & "
                            + String.format("%.6f", v.getR()) + " & "
                            + String.format("%.6f", v.getM()) + " & "
                            + String.format("%.10f", v.getCurrentMin().y));
            if (i != 0) {
                ParabolaIterationInfo prev = result.parabolas.get(i - 1);
                System.out.println(" & " + String.format("%.6f", (v.getR() - v.getL()) / (prev.getR() - prev.getL())) + " \\\\");
            } else {
                System.out.println(" & " + String.format("%.6f", 1.) + " \\\\");
            }
        }
        System.out.println("\\bottomrule\n" +
                "\\end{tabular}\n" +
                "\\caption{Parabola}\n" +
                "\\end{table}");
    }

    private static String pointToString(Point p) {
        return String.format("%.6f", p.getX()) + " & " + String.format("%.6f", p.getY());
    }

    private static void writeBrent() {
        MinimizationResult result = service.findMin(
                expr,
                -0.5,
                0.5,
                eps,
                MethodEnum.BRENT);
        System.out.println("\\begin{table}[h]\n" +
                "\\center\n" +
                "\\begin{tabular}{cccccccc}\n" +
                "\\toprule");
        System.out.println("left & right & $x_m$ & $y_m$ & min & attitude \\\\");
        System.out.println("\\midrule");
        for (int i = 0; i < result.segments.size(); i++) {
            SegmentIterationInfo v = result.segments.get(i);
            System.out.print(
                    String.format("%.6f", v.getL()) + " & "
                            + String.format("%.6f", v.getR()) + " & "
                            + String.format("%.6f", v.getX1Point().x) + " & "
                            + String.format("%.6f", v.getX1Point().y) + " & "
                            + String.format("%.10f", v.getCurrentMin().y));
            if (i != 0) {
                SegmentIterationInfo prev = result.segments.get(i - 1);
                System.out.println(" & " + String.format("%.6f", (v.getR() - v.getL()) / (prev.getR() - prev.getL())) + " \\\\");
            } else {
                System.out.println(" & " + String.format("%.6f", 1.) + " \\\\");
            }
        }
        System.out.println("\\bottomrule\n" +
                "\\end{tabular}\n" +
                "\\caption{Brent}\n" +
                "\\end{table}");
    }
}
