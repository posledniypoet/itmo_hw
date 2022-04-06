#include "calc.h"
#include <cctype> // for std::isspace
#include <cmath> // various math functions
#include <iostream> // for error reporting via std::cerr

const std::size_t ER = 0 , UN = 1 , BIN = 2 , ROL = 3;
namespace {
    const std::size_t max_decimal_digits = 10;
    enum class Op {
        ERR, SET, ADD, SUB, MUL, DIV, REM, NEG, POW, SQRT, ROLLED_ADD, ROLLED_SUB, ROLLED_MUL, ROLLED_POW, ROLLED_DIV, ROLLED_REM
    };

    std::size_t type_op(const Op op) {
        switch(op) {

            // error
            case Op:: ERR: return  ER;
                // unary
            case Op::NEG: return UN;
            case Op::SQRT: return UN;
                // binary
            case Op::SET: return BIN;
            case Op::ADD: return BIN;
            case Op::SUB: return BIN;
            case Op::MUL: return BIN;
            case Op::DIV: return BIN;
            case Op::REM: return BIN;
            case Op::POW: return BIN;
            case Op::ROLLED_ADD: return ROL;
            case Op::ROLLED_SUB: return ROL;
            case Op::ROLLED_MUL: return ROL;
            case Op::ROLLED_POW: return ROL;
            case Op::ROLLED_DIV: return ROL;
            case Op::ROLLED_REM:return ROL;}
        return 0;
    }

    Op parse_op(const std::string &line, std::size_t &i) {
        const auto rollback = [&i, &line](const std::size_t n) {
            i -= n;
            std::cerr << "Unknown operation " << line << std::endl;
            return Op::ERR;
        };
        switch (line[i++]) {
            case '0': [[fallthrough]];
            case '1': [[fallthrough]];
            case '2': [[fallthrough]];
            case '3': [[fallthrough]];
            case '4': [[fallthrough]];
            case '5': [[fallthrough]];
            case '6': [[fallthrough]];
            case '7': [[fallthrough]];
            case '8': [[fallthrough]];
            case '9':
                --i; // a first digit is a part of op's argument
                return Op::SET;
            case '+':
                return Op::ADD;
            case '-':
                return Op::SUB;
            case '*':
                return Op::MUL;
            case '/':
                return Op::DIV;
            case '%':
                return Op::REM;
            case '_':
                return Op::NEG;
            case '^':
                return Op::POW;
            case '(':
                switch (line[i++]) {
                    case '+':
                        switch (line[i++]) {
                            case ')':
                                return Op::ROLLED_ADD;
                            default: rollback(3);
                        }
                    case '*':
                        switch (line[i++]) {
                            case ')' :
                                return Op::ROLLED_MUL;
                            default:
                                rollback(3);
                        }
                    case '-':
                        switch (line[i++]) {
                            case ')' :
                                return Op::ROLLED_SUB;
                            default:
                                rollback(3);
                        }
                    case '^':
                        switch (line[i++]) {
                            case ')' :
                                return Op::ROLLED_POW;
                            default:
                                rollback(3);
                        }
                    case '/':
                        switch (line[i++]) {
                            case ')' :
                                return Op::ROLLED_DIV;
                            default:
                                rollback(3);
                        }
                    case '%':
                        switch (line[i++]) {
                            case ')' :
                                return Op::ROLLED_REM;
                            default:
                                rollback(3);
                        }
                    default:
                        rollback(2);
                }
            case 'S':
                switch (line[i++]) {
                    case 'Q':
                        switch (line[i++]) {
                            case 'R':
                                switch (line[i++]) {
                                    case 'T':
                                        return Op::SQRT;
                                    default:
                                        return rollback(4);
                                }
                            default:
                                return rollback(3);
                        }
                    default:
                        return rollback(2);
                }
            default:
                return rollback(1);
        }
    }
    std::size_t skip_ws(const std::string &line, std::size_t &i)
    {

        while (i < line.size() && std::isspace(line[i])) {
            ++i;
        }
        return i;
    }
    double parse_arg(const std::string &line, std::size_t &i) {
        double res = 0;
        std::size_t count = 0;
        bool good = true;
        bool integer = true;
        double fraction = 1;
        skip_ws(line, i);
        while (good && i < line.size() && count < max_decimal_digits) {
            switch (line[i]) {
                case '0': [[fallthrough]];
                case '1': [[fallthrough]];
                case '2': [[fallthrough]];
                case '3': [[fallthrough]];
                case '4': [[fallthrough]];
                case '5': [[fallthrough]];
                case '6': [[fallthrough]];
                case '7': [[fallthrough]];
                case '8': [[fallthrough]];
                case '9':
                    if (integer) {
                        res *= 10;
                        res += line[i] - '0';
                    } else {
                        fraction /= 10;
                        res += (line[i] - '0') * fraction;
                    }
                    ++i;
                    ++count;
                    break;
                case '.':
                    integer = false;
                    ++i;
                    break;
                default:
                    good = false;
                    break;
            }
        }
        if ((i < line.size() && count == 0) || count == max_decimal_digits) {
            std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
        }
        return res;
    }
    double unary(const double current, const Op op) {
        switch (op) {
            case Op::NEG:
                return -current;
            case Op::SQRT:
                if (current > 0) {
                    return std::sqrt(current);
                } else {
                    std::cerr << "Bad argument for SQRT: " << current << std::endl;
                    [[fallthrough]];
                }
            default:
                return current;
        }
    }
    double binary(const Op op, const double left, const double right) {
        switch (op) {
            case Op::SET:
                return right;
            case Op::ADD:
            case Op::ROLLED_ADD:
                return left+right;
            case Op::SUB:
            case Op::ROLLED_SUB:
                return left - right;
            case Op::MUL:
            case Op::ROLLED_MUL:
                return left*right;
            case Op::DIV:
            case Op::ROLLED_DIV:
                if (right != 0) {
                    return left / right;
                } else {
                    std::cerr << "Bad right argument for division: " << right << std::endl;
                    return left;
                }
            case Op::REM:
                if (right != 0) {
                    return std::remainder(left, right);
                } else {
                    std::cerr << "Bad right argument for remainder: " << right << std::endl;
                    return left;
                }
            case Op::POW:
            case Op::ROLLED_POW:
                return std::pow(left, right);
            case Op::ROLLED_REM:
                if (right != 0) {
                    return std::fmod(left, right);
                } else {
                    std::cerr << "Bad right argument for remainder: " << right << std::endl;
                    return left;
                }
            default:
                return left;
        }
    }



}
double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (type_op(op)) {
        case BIN: {
            i = skip_ws(line, i);
            const auto arg = parse_arg(line, i);
            return binary(op, current, arg);
        }
        case UN: return unary(current, op);
        case ROL:
            std::size_t j = i;
            auto left = current;
            auto right = parse_arg(line, i);
            while (j != i) {
                j = i;
                left = binary(op, left, right);
                right = parse_arg(line, i);
            }
            return left;
    }
    return current;
}