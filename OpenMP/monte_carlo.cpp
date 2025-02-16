#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <random>
#include <omp.h>

double calculateIntegral(const InputData& data) {
    double sum = 0.0;
    double range = data.upper_bound - data.lower_bound;
    int num_threads = omp_get_max_threads();
    std::vector<double> partial_sums(num_threads, 0.0);

    #pragma omp parallel
    {
        int thread_id = omp_get_thread_num();
        std::random_device rd;
        std::mt19937 gen(rd() + thread_id);
        std::uniform_real_distribution<double> dis(data.lower_bound, data.upper_bound);

        std::vector<double> local_point(data.variables);

        #pragma omp for
        for (int i = 0; i < data.points; ++i) {
            for (int j = 0; j < data.variables; ++j) {
                local_point[j] = dis(gen);
            }
            partial_sums[thread_id] += data.function.evaluate(local_point);
        }
    }

    for (double partial_sum : partial_sums) {
        sum += partial_sum;
    }

    double volume = data.variables == 1 ? range : range * range;
    return (sum / data.points) * volume;
}
