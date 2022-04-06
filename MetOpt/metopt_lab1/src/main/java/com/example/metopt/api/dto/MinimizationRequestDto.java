package com.example.metopt.api.dto;

import com.example.metopt.math.minimization.MethodEnum;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 08.03.2021
 */

public class MinimizationRequestDto {

    @JsonProperty("formula")
    private String formula;
    @JsonProperty("l")
    private double l;
    @JsonProperty("r")
    private double r;
    @JsonProperty("inaccuracy")
    private double inaccuracy;
    @JsonProperty("method")
    private MethodEnum method;

    public String getFormula() {
        return formula;
    }

    public void setFormula(String function) {
        this.formula = function;
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

    public MethodEnum getMethod() {
        return method;
    }

    public void setMethod(MethodEnum method) {
        this.method = method;
    }
}
