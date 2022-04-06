package com.example.metopt.standalone;

import com.example.metopt.api.ApiSpringConfiguration;
import com.example.metopt.application.ApplicationSpringConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 28.02.2021
 */

@Configuration
@Import({ApiSpringConfiguration.class, ApplicationSpringConfiguration.class})
public class StandaloneSpringConfiguration {
}
