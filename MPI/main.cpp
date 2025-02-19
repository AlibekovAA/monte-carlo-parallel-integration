#include "monte_carlo.h"
#include <iostream>
#include <windows.h>
#include <iomanip>
#include <ctime>
#include <mpi.h>
#include <thread>

int main(int argc, char* argv[]) {
    MPI_Init(&argc, &argv);

    int rank, size, max_processes;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int name_len;
    MPI_Get_processor_name(processor_name, &name_len);

    max_processes = std::thread::hardware_concurrency();

    SetConsoleOutputCP(CP_UTF8);

    try {
        InputData data = parseInputFile("input.txt");

        int requested_processes = std::stoi(data.count_processes);
        if (requested_processes > max_processes) {
            if (rank == 0) {
                std::cout << "Предупреждение: Запрошенное количество процессов (" << requested_processes
                          << ") превышает доступное (" << max_processes
                          << "). Будет использовано максимально доступное количество." << std::endl;
            }
            requested_processes = max_processes;
        }

        if (rank == 0) {
            std::cout << "Функция: " << data.function.getExpression() << std::endl;
            std::cout << "Количество переменных: " << data.variables << std::endl;
            std::cout << "Границы: " << data.lower_bound << " " << data.upper_bound << std::endl;
            std::cout << "Количество точек: " << data.points << std::endl;
            std::cout << "Доступно процессоров: " << max_processes << std::endl;
            std::cout << "Используется процессов: " << size << std::endl;
            std::cout << "Точек на процесс: " << data.points / size <<
                (data.points % size ? " (+" + std::to_string(data.points % size) + " для последнего)" : "") << std::endl;
        }

        double start_time = MPI_Wtime();
        double result = calculateIntegral(data);
        double end_time = MPI_Wtime();

        if (rank == 0) {
            std::cout << "\nРезультат интегрирования: " << std::fixed << std::setprecision(5) << result << std::endl;
            std::cout << "Время выполнения: " << std::fixed << std::setprecision(3)
                      << (end_time - start_time) << " секунд" << std::endl;
        }
    }
    catch (const std::exception& e) {
        if (rank == 0) {
            std::cerr << "Ошибка: " << e.what() << std::endl;
        }
        MPI_Finalize();
        return 0;
    }

    MPI_Finalize();
    return 0;
}
