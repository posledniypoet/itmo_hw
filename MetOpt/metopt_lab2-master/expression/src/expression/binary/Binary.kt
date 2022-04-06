package expression.binary

import kotlin.math.pow

import expression.Expression
import expression.nullary.Variable
import expression.unary.Unary.*

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */
@Suppress("unused")
enum class Binary(
    val token: String,
    private val calculator: (Double, Double) -> Double,
    private val differentiator: (Expression, Expression, Variable) -> Expression,
    private val creator: (Expression, Expression) -> Expression,
) {
    ADD(
        "+",
        Double::plus,
        { left, right, variable ->
            ADD(
                left.diff(variable),
                right.diff(variable),
            )
        },
        { left, right -> buildCreator(ADD, left, right) },
    ),

    SUB(
        "-",
        Double::minus,
        { left, right, variable ->
            SUB(
                left.diff(variable),
                right.diff(variable),
            )
        },
        { left, right -> buildCreator(SUB, left, right) },
    ),

    MUL(
        "*",
        Double::times,
        { left, right, variable ->
            ADD(
                MUL(left.diff(variable), right),
                MUL(left, right.diff(variable)),
            )
        },
        { left, right -> buildCreator(MUL, left, right) },
    ),

    DIV(
        "/",
        Double::div,
        { left, right, variable ->
            DIV(
                SUB(
                    MUL(left.diff(variable), right),
                    MUL(left, right.diff(variable)),
                ),
                MUL(right, right),
            )
        },
        { left, right -> buildCreator(DIV, left, right) },
    ),

    POW(
    "^",
    Double::pow,
    { left, right, variable ->
        MUL(
            POW(left, right),
            ADD(
                MUL(
                    DIV(right, left),
                    left.diff(variable),
                ),
                MUL(
                    LN(left),
                    right.diff(variable)
                ),
            ),
        )
    },
    { left, right -> buildCreator(POW, left, right) },
    );

    operator fun invoke(left: Expression, right: Expression) = creator(left, right)

    private companion object {

        fun buildCreator(
            operation: Binary,
            left: Expression,
            right: Expression,
        ): Expression =
            object : Expression {

                override fun evaluate(vars: Map<Variable, Double>): Double =
                    operation.calculator(left.evaluate(vars), right.evaluate(vars))

                override fun diff(variable: Variable): Expression =
                    operation.differentiator(left, right, variable)

                override fun toString(): String =
                    "$left ${operation.token} $right"
            }
    }
}
