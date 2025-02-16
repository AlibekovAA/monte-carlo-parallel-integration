#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <stdexcept>

double calculateIntegral(const InputData& data) {
    srand(time(0));

    double sum = 0.0;
    std::vector<double> point(data.variables);
    double range = data.upper_bound - data.lower_bound;

    for (int i = 0; i < data.points; ++i) {
        for (int j = 0; j < data.variables; ++j) {
            point[j] = data.lower_bound + (rand() / (double)RAND_MAX) * range;
        }
        sum += data.function.evaluate(point);
    }

    double volume = data.variables == 1 ?
        range :
        range * range;

    return (sum / data.points) * volume;
}
