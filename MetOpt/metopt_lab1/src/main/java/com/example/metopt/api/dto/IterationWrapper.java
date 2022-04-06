package com.example.metopt.api.dto;

import java.util.List;

import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.entity.Pair;

public class IterationWrapper {

    private final DrawType type;
    private final Pair segment1;
    private final Pair segment2;
    private final List<Point> parabola;

    public IterationWrapper(DrawType type, Pair segment1, Pair segment2, List<Point> parabola) {
        this.type = type;
        this.segment1 = segment1;
        this.segment2 = segment2;
        this.parabola = parabola;
    }

    public DrawType getType() {
        return type;
    }

    public Pair getSegment1() {
        return segment1;
    }

    public Pair getSegment2() {
        return segment2;
    }

    public List<Point> getParabola() {
        return parabola;
    }
}
