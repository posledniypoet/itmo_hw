package com.example.metopt.api.dto;

import java.util.List;

import com.example.metopt.math.geometry.Point;

public class EvaluateResultDto {

    String formula;
    List<Point> points;

    public EvaluateResultDto(String formula, List<Point> points) {
        this.formula = formula;
        this.points = points;
    }

    public String getFormula() {
        return formula;
    }

    public void setFormula(String formula) {
        this.formula = formula;
    }

    public List<Point> getPoints() {
        return points;
    }

    public void setPoints(List<Point> points) {
        this.points = points;
    }
}
