package com.example.metopt.api.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping(value = {"", "/", "/index"})
public class PagesController {

    @GetMapping("")
    public String indexPage() {
        return "indexPage";
    }

    @GetMapping("/lab1")
    public String firstTaskPage() {
        return "firstTaskPage";
    }
}
