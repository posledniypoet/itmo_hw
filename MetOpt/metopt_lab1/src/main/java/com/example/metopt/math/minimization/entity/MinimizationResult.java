package com.example.metopt.math.minimization.entity;

import com.example.metopt.math.geometry.Point;

import java.util.List;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class MinimizationResult {

    public final Point min;
    public final List<Point> function;
    public final int calcCount;
    public final List<SegmentIterationInfo> segments;
    public final List<List<Point>> parabolasGraphics;
    public final List<ParabolaIterationInfo> parabolas;

    public MinimizationResult(Point min,
                              List<Point> function,
                              int calcCount,
                              List<SegmentIterationInfo> segments,
                              List<List<Point>> parabolasGraphics,
                              List<ParabolaIterationInfo> parabolas) {
        this.min = min;
        this.function = function;
        this.calcCount = calcCount;
        this.segments = segments;
        this.parabolasGraphics = parabolasGraphics;
        this.parabolas = parabolas;
    }
}
