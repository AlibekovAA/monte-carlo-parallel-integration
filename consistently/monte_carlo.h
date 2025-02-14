#ifndef MONTE_CARLO_H
#define MONTE_CARLO_H

#include <string>

struct InputData {
    std::string function;
    int variables;
    double lower_bound;
    double upper_bound;
    int points;
};

InputData parseInputFile(const std::string& filename);

#endif
