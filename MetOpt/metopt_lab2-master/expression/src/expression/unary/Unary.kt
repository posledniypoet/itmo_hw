package expression.unary

import kotlin.math.cos
import kotlin.math.exp
import kotlin.math.sin

import expression.Expression
import expression.nullary.Variable
import expression.binary.Binary.*
import kotlin.math.ln

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */
enum class Unary(
    val token: String,
    private val calculator: (Double) -> Double,
    private val differentiator: (Expression, Variable) -> Expression,
    private val creator: (Expression) -> Expression,
) {
    NEGATE(
        "-",
        Double::unaryMinus,
        { arg, variable ->
            NEGATE(arg.diff(variable))
        },
        { arg -> buildCreator(NEGATE, arg) }
    ),

    SINES(
        "sin",
        ::sin,
        { arg, variable ->
            COSINE(arg.diff(variable))
        },
        { arg -> buildCreator(SINES, arg) }
    ),

    COSINE(
        "cos",
        ::cos,
        { arg, variable ->
            NEGATE(SINES(arg.diff(variable)))
        },
        { arg -> buildCreator(COSINE, arg) }
    ),

    EXP(
        "exp",
        ::exp,
        { arg, variable ->
            MUL(
                EXP(arg),
                arg.diff(variable)
            )
        },
        { arg -> buildCreator(EXP, arg) }
    ),

    LN(
    "ln",
    ::ln,
    { arg, variable ->
        DIV(arg.diff(variable), arg)
    },
    { arg -> buildCreator(LN, arg) }
    );

    operator fun invoke(arg: Expression) = this.creator(arg)

    private companion object {

        fun buildCreator(
            operation: Unary,
            arg: Expression,
        ): Expression =
            object : Expression {

                override fun evaluate(vars: Map<Variable, Double>): Double =
                    operation.calculator(arg.evaluate(vars))

                override fun diff(variable: Variable): Expression =
                    operation.differentiator(arg.diff(variable), variable)

                override fun toString(): String =
                    "${operation.token} $arg"
            }
    }
}
