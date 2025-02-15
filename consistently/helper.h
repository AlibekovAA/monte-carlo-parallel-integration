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

struct InputData {
    std::string function;
    int variables;
    double lower_bound;
    double upper_bound;
    int points;
};

InputData parseInputFile(const std::string& filename);
double evaluateTerm(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos);
double evaluateExpression(const std::string& expr, const std::vector<double>& values, const std::vector<std::string>& varNames, size_t& pos);
double evaluateFunction(const std::string& function, const std::vector<double>& point);

#endif
