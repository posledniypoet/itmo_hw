package expression.parser

import expression.Expression
import expression.binary.Binary
import expression.binary.Binary.*
import expression.nullary.Const
import expression.nullary.Variable
import expression.unary.Unary
import expression.unary.Unary.*

/**
 * @author Danil Demintsev (demintsievd@yandex.ru) on 26.02.2021
 */
class ExpressionParser(
    expr: String
) {

    private val expr = expr.replace("\\s+".toRegex(), "")

    private var pos = 0

    fun parse(): Expression {
        val result: Expression = parseBinary(0)
        if (!isEnd()) {
            throw IllegalArgumentException("Not everything has been parsed")
        }
        return result
    }

    private fun parseBinary(priority: Int): Expression {
        if (priority >= binaryPriority.size) {
            return parseUnary()
        }
        var left: Expression = parseBinary(priority + 1)
        while (!isEnd()) {
            val operation: Binary = parseOperation(priority) ?: return left
            val right: Expression = parseBinary(priority + 1)
            left = operation(left, right)
        }
        return left
    }

    private fun parseUnary(): Expression {
        for (operation in unary) {
            if (test(operation.token)) {
                return operation(parseUnary())
            }
        }
        return parseOperand()
    }

    private fun parseOperand(): Expression {
        for (name in Variable.values()) {
            if (test(name.token)) {
                return name
            }
        }
        if (test('(')) {
            return parseParenthesis()
        }
        val index = pos++
        while (pos < expr.length && ('0' <= expr[pos] && expr[pos] <= '9' || expr[pos] == '.')) {
            ++pos
        }
        return Const(expr.substring(index, pos).toDouble())
    }

    private fun parseParenthesis(): Expression {
        val result: Expression = parseBinary(0)
        if (test(')')) {
            return result
        }
        throw IllegalArgumentException("')' expected on pos " + pos + " but '" + expr[pos] + "' found")
    }

    private fun parseOperation(priority: Int): Binary? {
        for (operation in binaryPriority[priority]) {
            if (test(operation.token)) {
                return operation
            }
        }
        return null
    }

    private fun test(str: String): Boolean {
        if (pos < expr.length && expr.startsWith(str, pos)) {
            pos += str.length
            return true
        }
        return false
    }

    private fun test(c: Char): Boolean {
        if (expr[pos] == c) {
            ++pos
            return true
        }
        return false
    }

    private fun isEnd(): Boolean {
        return pos == expr.length
    }

    companion object {

        private val binaryPriority: List<List<Binary>> = listOf(
            listOf(ADD, SUB),
            listOf(MUL, DIV),
            listOf(POW)
        )

        private val unary: List<Unary> = listOf(
            NEGATE,
            SINES,
            COSINE
        )
    }
}
