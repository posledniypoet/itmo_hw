package com.example.metopt.math.minimization.entity;

import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.geometry.Point;

public class ParabolaIterationInfo {

    private final double l;
    private final double m;
    private final double r;
    private final Point currentMin;
    private final Expression parabola;
    private final int index;

    public ParabolaIterationInfo(double l, double m, double r, Point currentMin, Expression parabola, int index) {
        this.l = l;
        this.m = m;
        this.r = r;
        this.currentMin = currentMin;
        this.parabola = parabola;
        this.index = index;
    }

    public double getL() {
        return l;
    }

    public double getM() {
        return m;
    }

    public double getR() {
        return r;
    }

    public Point getCurrentMin() {
        return currentMin;
    }

    public Expression getParabola() {
        return parabola;
    }

    public int getIndex() {
        return index;
    }
}
