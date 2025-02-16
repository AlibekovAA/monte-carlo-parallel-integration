#include "monte_carlo.h"
#include <iostream>
#include <windows.h>
#include <iomanip>
#include <ctime>
#include <omp.h>

int main() {
    SetConsoleOutputCP(CP_UTF8);

    try {
        InputData data = parseInputFile("input.txt");
        int max_threads = omp_get_max_threads();
        int num_threads;

        if (data.count_threads == "max") {
            num_threads = max_threads;
        } else {
            num_threads = std::stoi(data.count_threads);
            if (num_threads > max_threads) {
                std::cout << "Предупреждение: Запрошенное количество потоков (" << num_threads
                          << ") превышает максимально доступное (" << max_threads
                          << "). Будет использовано " << max_threads << " потоков." << std::endl;
                num_threads = max_threads;
            }
        }

        omp_set_num_threads(num_threads);

        std::cout << "Функция: " << data.function.getExpression() << std::endl;
        std::cout << "Количество переменных: " << data.variables << std::endl;
        std::cout << "Границы: " << data.lower_bound << " " << data.upper_bound << std::endl;
        std::cout << "Количество точек: " << data.points << std::endl;
        std::cout << "Используется потоков: " << num_threads << std::endl;

        clock_t start = clock();
        double result = calculateIntegral(data);
        clock_t end = clock();

        double time_spent = (double)(end - start) / CLOCKS_PER_SEC;

        std::cout << "\nРезультат интегрирования: " << std::fixed << std::setprecision(5) << result << std::endl;
        std::cout << "Время выполнения: " << std::fixed << std::setprecision(3) << time_spent << " секунд" << std::endl;
    }
    catch (const std::exception& e) {
        std::cerr << "Ошибка: " << e.what() << std::endl;
        return 0;
    }

    return 0;
}
