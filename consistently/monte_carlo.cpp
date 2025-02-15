#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <stdexcept>
#include <random>

double calculateIntegral(const InputData& data) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(data.lower_bound, data.upper_bound);

    double sum = 0.0;
    std::vector<double> point(data.variables);

    for (int i = 0; i < data.points; ++i) {
        for (int j = 0; j < data.variables; ++j) {
            point[j] = dis(gen);
        }
        sum += evaluateFunction(data.function, point);
    }

    double volume = data.variables == 1 ?
        (data.upper_bound - data.lower_bound) :
        pow(data.upper_bound - data.lower_bound, 2);

    return (sum / data.points) * volume;
}
