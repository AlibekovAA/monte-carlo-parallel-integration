#include <omp.h>
#include <iostream>
#include <iomanip>
#include <windows.h>

int main() {
    SetConsoleOutputCP(CP_UTF8);
    std::cout << "Всего доступно " << omp_get_max_threads() << " потоков\n\n";

    #pragma omp parallel
    {
        #pragma omp critical
        {
            std::cout << "Поток " << std::setw(2) << omp_get_thread_num()
                      << " из " << omp_get_num_threads() << " запущен\n";
        }
    }
    return 0;
}
