#pragma once
#include <omp.h>

#ifndef HELPER_H
#define HELPER_H

#define _USE_MATH_DEFINES
#include <cmath>

#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <stdexcept>

namespace math {
    const std::map<std::string, double> CONSTANTS = {
        {"pi", M_PI},
        {"e", M_E}
    };

    const std::map<std::string, double(*)(double)> FUNCTIONS = {
        {"sin", std::sin},
        {"cos", std::cos},
        {"exp", std::exp},
        {"log", std::log}
    };
}

class CompiledFunction {
private:
    std::string expr;
    std::vector<std::string> varNames;

public:
    CompiledFunction(const std::string& expression = "0", int numVariables = 1)
        : expr(expression) {
        for (int i = 1; i <= numVariables; i++) {
            varNames.push_back("x" + std::to_string(i));
        }
    }

    double evaluate(const std::vector<double>& values) const;
    const std::string& getExpression() const { return expr; }
};

struct InputData {
    CompiledFunction function;
    int variables;
    double lower_bound;
    double upper_bound;
    int points;

    InputData() : function("0", 1), variables(1), lower_bound(0), upper_bound(0), points(0) {}
};

InputData parseInputFile(const std::string& filename);
double evaluateTerm(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos);
double evaluateExpression(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos);

double function(double x);
double random_double(unsigned int* seed);

#endif
