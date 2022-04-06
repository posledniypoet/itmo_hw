package com.example.metopt.api.dto;

import java.util.List;

public class EvaluateRequestDto {

    private String formula;
    private List<Double> x;

    public EvaluateRequestDto(String formula, List<Double> x) {
        this.formula = formula;
        this.x = x;
    }

    public String getFormula() {
        return formula;
    }

    public void setFormula(String formula) {
        this.formula = formula;
    }

    public List<Double> getX() {
        return x;
    }

    public void setX(List<Double> x) {
        this.x = x;
    }
}
