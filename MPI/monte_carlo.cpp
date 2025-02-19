#include "monte_carlo.h"
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <random>
#include <mpi.h>

double calculateIntegral(const InputData& data) {
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    double local_sum = 0.0;
    double global_sum = 0.0;
    double range = data.upper_bound - data.lower_bound;

    int points_per_process = data.points / size;
    if (rank == size - 1) {
        points_per_process += data.points % size;
    }

    std::random_device rd;
    std::mt19937 gen(rd() + rank);
    std::uniform_real_distribution<double> dis(data.lower_bound, data.upper_bound);

    std::vector<double> point(data.variables);

    for (int i = 0; i < points_per_process; ++i) {
        for (int j = 0; j < data.variables; ++j) {
            point[j] = dis(gen);
        }
        local_sum += data.function.evaluate(point);
    }

    MPI_Reduce(&local_sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    double volume = data.variables == 1 ? range : range * range;
    return (global_sum / data.points) * volume;
}
