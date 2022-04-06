package com.example.metopt.math.minimization.entity;

import java.util.function.BinaryOperator;

import com.example.metopt.math.geometry.Point;

public class SegmentIterationInfo {

    private final double l;
    private final double r;
    private final Point x1Point;
    private final Point x2Point;
    private final Point currentMin;
    private final Pair x1ToDraw;
    private final Pair x2ToDraw;
    private final int index;

    public SegmentIterationInfo(double l, double r, Point x1Point, Point x2Point, Pair x1ToDraw, Pair x2ToDraw, int index) {
        this.l = l;
        this.r = r;
        this.x1Point = x1Point;
        this.x2Point = x2Point;
        this.currentMin = BinaryOperator.minBy(Point.cmpByY).apply(x1Point, x2Point);
        this.x1ToDraw = x1ToDraw;
        this.x2ToDraw = x2ToDraw;
        this.index = index;
    }

    public double getL() {
        return l;
    }

    public double getR() {
        return r;
    }

    public Point getX1Point() {
        return x1Point;
    }

    public Point getX2Point() {
        return x2Point;
    }

    public Pair getX1ToDraw() {
        return x1ToDraw;
    }

    public Pair getX2ToDraw() {
        return x2ToDraw;
    }

    public Point getCurrentMin() {
        return currentMin;
    }

    public int getIndex() {
        return index;
    }
}
