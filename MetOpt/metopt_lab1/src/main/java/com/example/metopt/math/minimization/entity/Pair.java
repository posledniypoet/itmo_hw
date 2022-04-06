package com.example.metopt.math.minimization.entity;

import com.example.metopt.math.geometry.Point;

public class Pair {

    public final Point fst;
    public final Point snd;
    public final Point value;

    public Pair(Point fst, Point snd, Point value) {
        this.fst = fst;
        this.snd = snd;
        this.value = value;
    }

    public Point getFst() {
        return fst;
    }

    public Point getSnd() {
        return snd;
    }

    public Point getValue() {
        return value;
    }
}
