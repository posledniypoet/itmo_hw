package com.example.metopt.api.controller;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.stream.Collectors;

import com.example.metopt.api.dto.DrawType;
import com.example.metopt.api.dto.EvaluateRequestDto;
import com.example.metopt.api.dto.EvaluateResultDto;
import com.example.metopt.api.dto.IterationWrapper;
import com.example.metopt.api.dto.MinimizationRequestDto;
import com.example.metopt.api.dto.MinimizationResultDto;
import com.example.metopt.application.service.FirstTaskService;
import com.example.metopt.math.expression.Expression;
import com.example.metopt.math.expression.operations.VariableName;
import com.example.metopt.math.geometry.Point;
import com.example.metopt.math.minimization.entity.ParabolaIterationInfo;
import com.example.metopt.math.minimization.entity.SegmentIterationInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru), Daniel Polzik (polzikd@mail.ru) on 28.02.2021
 */

@Controller
@RequestMapping(value = "/lab1")
public class FirstTaskApiController {

    private final FirstTaskService service;

    @Autowired
    public FirstTaskApiController(FirstTaskService service) {
        this.service = service;
    }

    @PostMapping(
            path = "/minimize",
            consumes = "application/json",
            produces = "application/json"
    )
    @ResponseBody
    public MinimizationResultDto minimize(@RequestBody MinimizationRequestDto request) {
        Expression expr = service.parseFormula(request.getFormula());
        var minResult = service.findMin(expr, request.getL(), request.getR(), request.getInaccuracy(),
                request.getMethod());
        List<IterationWrapper> iterations = new ArrayList<>(minResult.segments.size() + minResult.parabolas.size());
        for (int i = 0; i < minResult.segments.size() + minResult.parabolas.size(); ++i) {
            iterations.add(null);
        }
        for (SegmentIterationInfo info : minResult.segments) {
            iterations.set(info.getIndex(), new IterationWrapper(DrawType.SEGMENT, info.getX1ToDraw(), info.getX2ToDraw(), null));
        }
        for (int i = 0; i < minResult.parabolas.size(); ++i) {
            var info = minResult.parabolas.get(i);
            var list = minResult.parabolasGraphics.get(i);
            iterations.set(info.getIndex(), new IterationWrapper(DrawType.PARABOLA, null, null, list));
        }
        return new MinimizationResultDto(
                request.getFormula(),
                request.getL(),
                request.getR(),
                request.getInaccuracy(),
                minResult.min,
                minResult.function,
                minResult.segments.stream()
                        .map(__ -> List.of(__.getX1ToDraw(), __.getX2ToDraw()))
                        .flatMap(List::stream)
                        .collect(Collectors.toList()),
                minResult.parabolasGraphics,
                iterations
        );
    }

    @PostMapping(
            path = "/evaluate",
            consumes = "application/json",
            produces = "application/json"
    )
    @ResponseBody
    public EvaluateResultDto evaluate(@RequestBody EvaluateRequestDto request) {
        final Expression expr = service.parseFormula(request.getFormula());
        final EnumMap<VariableName, Double> vars = new EnumMap<>(VariableName.class);
        return new EvaluateResultDto(
                request.getFormula(),
                request.getX().stream()
                        .map(x -> {
                            vars.put(VariableName.X, x);
                            return new Point(x, expr.evaluate(vars));
                        }).collect(Collectors.toList())
        );
    }
}
