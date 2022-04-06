package expression.nullary

import expression.Expression

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */
class Const(
    private val value: Double
) : Expression {

    override fun evaluate(vars: Map<Variable, Double>): Double = value

    override fun diff(variable: Variable): Expression = ZERO

    override fun toString(): String =
        value.toString()

    companion object {
        val ZERO = Const(0.0)
        val ONE = Const(1.0)
    }
}
