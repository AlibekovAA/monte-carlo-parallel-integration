#include "monte_carlo.h"
#include <iostream>
#include <windows.h>
#include <iomanip>

int main() {
    SetConsoleOutputCP(CP_UTF8);

    InputData data = parseInputFile("input.txt");

    std::cout << "Функция: " << data.function << std::endl;
    std::cout << "Количество переменных: " << data.variables << std::endl;
    std::cout << "Границы: " << data.lower_bound << " " << data.upper_bound << std::endl;
    std::cout << "Количество точек: " << data.points << std::endl;

    double result = calculateIntegral(data);
    std::cout << "\nРезультат интегрирования: " << std::fixed << std::setprecision(6) << result << std::endl;

    return 0;
}
