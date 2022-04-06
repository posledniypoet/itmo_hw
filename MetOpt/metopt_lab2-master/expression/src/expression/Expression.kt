package expression

import expression.nullary.Variable
import expression.parser.ExpressionParser

interface Expression {

    fun evaluate(vars: Map<Variable, Double>): Double

    fun diff(variable: Variable): Expression

    fun evaluate(vars: List<Pair<Variable, Double>>) =
        evaluate(vars.toMap())

    fun evaluate(vararg vars: Pair<Variable, Double>) =
        evaluate(vars.toList())

    companion object {
        fun parse(str: String): Expression =
            ExpressionParser(str).parse()
    }
}
