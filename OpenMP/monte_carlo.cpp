#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <random>
#include <omp.h>

double calculateIntegral(const InputData& data) {
    double sum = 0.0;
    double range = data.upper_bound - data.lower_bound;

    #pragma omp parallel
    {
        std::random_device rd;
        std::mt19937 gen(rd() + omp_get_thread_num());
        std::uniform_real_distribution<double> dis(data.lower_bound, data.upper_bound);

        std::vector<double> local_point(data.variables);
        double local_sum = 0.0;

        #pragma omp for
        for (int i = 0; i < data.points; ++i) {
            for (int j = 0; j < data.variables; ++j) {
                local_point[j] = dis(gen);
            }
            local_sum += data.function.evaluate(local_point);
        }

        #pragma omp atomic
        sum += local_sum;
    }

    double volume = data.variables == 1 ? range : range * range;
    return (sum / data.points) * volume;
}
