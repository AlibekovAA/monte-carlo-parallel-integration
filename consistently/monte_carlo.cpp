#include "monte_carlo.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

InputData parseInputFile(const std::string& filename) {
    InputData data;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string key;

        std::getline(iss, key, ':');
        if (key == "function") {
            std::getline(iss, data.function);
            data.function = data.function.substr(1);
        }
        else if (key == "variables") {
            iss >> data.variables;
        }
        else if (key == "bounds") {
            iss >> data.lower_bound >> data.upper_bound;
        }
        else if (key == "points") {
            iss >> data.points;
        }
    }

    return data;
}
