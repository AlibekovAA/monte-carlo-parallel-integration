#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>

double calculateIntegral(const InputData& data) {
    srand(time(0));
    double sum = 0.0;
    std::vector<double> point(data.variables);

    for (int i = 0; i < data.points; ++i) {
        for (int j = 0; j < data.variables; ++j) {
            double r = (double)rand() / RAND_MAX;
            point[j] = data.lower_bound + r * (data.upper_bound - data.lower_bound);
        }

        sum += evaluateFunction(data.function, point);
    }

    double volume = pow(data.upper_bound - data.lower_bound, data.variables);
    return (sum / data.points) * volume;
}
