package com.example.metopt.api.dto;

import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.entity.Pair;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 28.02.2021
 */

public class MinimizationResultDto {

    private String formula;
    private double l;
    private double r;
    private double inaccuracy;
    private Point min;
    private List<Point> function;
    private List<Pair> segments;
    private List<List<Point>> parabols;
    private List<IterationWrapper> iterations;

    public MinimizationResultDto(String formula,
                                 double l,
                                 double r,
                                 double inaccuracy,
                                 Point min,
                                 List<Point> function,
                                 List<Pair> segments,
                                 List<List<Point>> parabols,
                                 List<IterationWrapper> iterations) {
        this.formula = formula;
        this.l = l;
        this.r = r;
        this.inaccuracy = inaccuracy;
        this.min = min;
        this.function = function;
        this.segments = segments;
        this.parabols = parabols;
        this.iterations = iterations;
    }

    public String getFormula() {
        return formula;
    }

    public void setFormula(String formula) {
        this.formula = formula;
    }

    public double getL() {
        return l;
    }

    public void setL(double l) {
        this.l = l;
    }

    public double getR() {
        return r;
    }

    public void setR(double r) {
        this.r = r;
    }

    public double getInaccuracy() {
        return inaccuracy;
    }

    public void setInaccuracy(double inaccuracy) {
        this.inaccuracy = inaccuracy;
    }

    public Point getMin() {
        return min;
    }

    public void setMin(Point min) {
        this.min = min;
    }

    public List<Point> getFunction() {
        return function;
    }

    public void setFunction(List<Point> function) {
        this.function = function;
    }

    public List<Pair> getSegments() {
        return segments;
    }

    public void setSegments(List<Pair> segments) {
        this.segments = segments;
    }

    public List<List<Point>> getParabols() {
        return parabols;
    }

    public void setParabols(List<List<Point>> parabols) {
        this.parabols = parabols;
    }

    public List<IterationWrapper> getIterations() {
        return iterations;
    }

    public void setIterations(List<IterationWrapper> iterations) {
        this.iterations = iterations;
    }
}
