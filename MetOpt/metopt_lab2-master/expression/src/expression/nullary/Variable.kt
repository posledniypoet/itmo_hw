package expression.nullary

import expression.Expression

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */
enum class Variable(
    val token: String,
) : Expression {

    X("x"),
    Y("y"),
    Z("z");
    //TODO: x1, x2, x3, ...

    override fun evaluate(vars: Map<Variable, Double>): Double =
        vars[this] ?: throw IllegalArgumentException("No value for variable with name $this")

    override fun diff(variable: Variable): Expression =
        if (variable == this) {
            Const.ONE
        } else {
            Const.ZERO
        }

    override fun toString(): String = token
}
