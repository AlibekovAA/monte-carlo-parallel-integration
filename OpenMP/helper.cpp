#include "helper.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <cmath>

InputData parseInputFile(const std::string& filename) {
    InputData data;
    std::ifstream file(filename);
    std::string line, key, value;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::getline(iss, key, ':');
        std::getline(iss, value);

        iss.clear();
        iss.str(value);

        if (key == "function") {
            data.function = CompiledFunction(value.substr(1), data.variables);
        }
        else if (key == "variables") {
            iss >> data.variables;
            if (data.variables < 1 || data.variables > 2) {
                throw std::runtime_error("Поддерживаются только одинарные и двойные интегралы");
            }
        }
        else if (key == "bounds") {
            iss >> data.lower_bound >> data.upper_bound;
        }
        else if (key == "points") {
            iss >> data.points;
        }
    }

    data.function = CompiledFunction(data.function.getExpression(), data.variables);

    return data;
}

double evaluateTerm(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos) {
    double result = 0;

    while (pos < expr.length()) {
        if (expr[pos] == ' ') {
            pos++;
            continue;
        }

        if (std::isdigit(expr[pos]) || expr[pos] == '.') {
            result = 0;
            bool hasDecimal = false;
            double decimal = 1.0;

            while (pos < expr.length() && (std::isdigit(expr[pos]) || expr[pos] == '.')) {
                if (expr[pos] == '.') {
                    if (hasDecimal) {
                        throw std::runtime_error("Некорректное число: множественные десятичные точки");
                    }
                    hasDecimal = true;
                } else if (hasDecimal) {
                    decimal *= 0.1;
                    result += (expr[pos] - '0') * decimal;
                } else {
                    result = result * 10 + (expr[pos] - '0');
                }
                pos++;
            }

            if (pos < expr.length() && std::isalpha(expr[pos])) {
                return result * evaluateTerm(expr, values, varNames, pos);
            }
            return result;
        }

        if (expr[pos] == '-' && (pos == 0 || expr[pos-1] == '(' || expr[pos-1] == '*' || expr[pos-1] == '/')) {
            pos++;
            return -evaluateTerm(expr, values, varNames, pos);
        }

        if (std::isalpha(expr[pos])) {
            std::string name;
            while (pos < expr.length() && (std::isalpha(expr[pos]) || std::isdigit(expr[pos]))) {
                name += expr[pos];
                pos++;
            }

            auto constIt = math::CONSTANTS.find(name);
            if (constIt != math::CONSTANTS.end()) {
                return constIt->second;
            }

            auto funcIt = math::FUNCTIONS.find(name);
            if (funcIt != math::FUNCTIONS.end()) {
                if (pos < expr.length() && expr[pos] == '(') {
                    pos++;
                    double arg = evaluateExpression(expr, values, varNames, pos);
                    if (pos < expr.length() && expr[pos] == ')') {
                        pos++;
                        return funcIt->second(arg);
                    }
                    throw std::runtime_error("Ожидалась закрывающая скобка после " + name);
                }
                throw std::runtime_error("Ожидалась открывающая скобка после " + name);
            }

            auto varIt = std::find(varNames.begin(), varNames.end(), name);
            if (varIt != varNames.end()) {
                int index = std::distance(varNames.begin(), varIt);
                if (pos < expr.length() && expr[pos] == '^') {
                    pos++;
                    double power = evaluateTerm(expr, values, varNames, pos);
                    return std::pow(values[index], power);
                }
                return values[index];
            }

            throw std::runtime_error("Неизвестный идентификатор: " + name);
        }

        if (expr[pos] == '(') {
            pos++;
            result = evaluateExpression(expr, values, varNames, pos);
            if (pos < expr.length() && expr[pos] == ')') {
                pos++;
            }
            return result;
        }
    }

    return result;
}

double evaluateExpression(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos) {
    double result = evaluateTerm(expr, values, varNames, pos);

    while (pos < expr.length()) {
        if (expr[pos] == ' ') {
            pos++;
            continue;
        }

        if (expr[pos] == '+') {
            pos++;
            result += evaluateTerm(expr, values, varNames, pos);
        }
        else if (expr[pos] == '-') {
            pos++;
            result -= evaluateTerm(expr, values, varNames, pos);
        }
        else if (expr[pos] == '*') {
            pos++;
            result *= evaluateTerm(expr, values, varNames, pos);
        }
        else if (expr[pos] == '/') {
            pos++;
            double divisor = evaluateTerm(expr, values, varNames, pos);
            if (std::abs(divisor) < 1e-10) {
                throw std::runtime_error("Деление на ноль");
            }
            result /= divisor;
        }
        else if (expr[pos] == ')') {
            break;
        }
        else {
            pos++;
        }
    }

    return result;
}

double CompiledFunction::evaluate(const std::vector<double>& values) const {
    size_t pos = 0;
    return evaluateExpression(expr, values, varNames, pos);
}
