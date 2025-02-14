#ifndef HELPER_H
#define HELPER_H

#include <string>
#include <vector>

struct InputData {
    std::string function;
    int variables;
    double lower_bound;
    double upper_bound;
    int points;
};

InputData parseInputFile(const std::string& filename);
double evaluateFunction(const std::string& function, const std::vector<double>& point);

#endif
