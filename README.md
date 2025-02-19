# Численное интегрирование методом Монте-Карло

Проект реализует численное интегрирование методом Монте-Карло с использованием различных подходов к параллельным вычислениям.

## Реализации

- [x] Последовательная реализация
- [x] №1: Параллельное программирование в системах с общей памятью (OpenMP)
- [x] №2: Параллельное программирование в системах с распределенной памятью (MPI)
- [ ] №3: Функциональные языки параллельного программирования (Haskell и Erlang)
- [ ] №4: Параллельное программирование на языке FPTL

## Поддерживаемые операции

### Основные операторы
- Сложение: `+`
- Вычитание: `-`
- Умножение: `*`
- Деление: `/`
- Возведение в степень: `^`

### Тригонометрические функции
- Синус: `sin(x)` `2sin(x)`
- Косинус: `cos(x)` `3cos(x)`

### Другие функции
- Экспонента: `exp(x)`
- Натуральный логарифм: `log(x)`

### Константы
- π (пи): `pi`
- e (число Эйлера): `e`

## Структура проекта

- `consistently/` - последовательная реализация метода
  - `input.txt` - входные данные
  - `monte_carlo.h/cpp` - реализация метода
  - `main.cpp` - точка входа программы
  - `Makefile` - файл сборки проекта

## Сборка и запуск

Доступны следующие команды make:
- `make build` - сборка проекта
- `make run` - запуск программы
- `make clean` - очистка файлов сборки
- `make rebuild` - пересборка и запуск программы

## Пример работы OpenMP
![Пример работы OpenMP](.\OpenMP\plots\speedup_by_points_20250218_231306.png)
![Пример работы OpenMP](.\OpenMP\plots\speedup_by_threads_20250218_231306.png)
