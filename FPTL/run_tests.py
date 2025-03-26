import subprocess
import re
import datetime
import matplotlib.pyplot as plt


def run_monte_carlo(n_processes):
    command = f".\\fptl_max_performance.exe monte_carlo.fptl -n {n_processes} -i"

    results = []
    for _ in range(5):
        result = subprocess.run(command, shell=True, capture_output=True, text=True)
        output = result.stdout
        work_time_match = re.search(r"Useful work time: (\d+\.\d+)s", output)

        work_time = float(work_time_match.group(1))

        integral_match = re.search(r"-\d+\.\d+", output)

        integral_value = float(integral_match.group())
        results.append((work_time, integral_value))

    avg_time = sum(t for t, _ in results) / len(results)
    avg_integral = sum(i for _, i in results) / len(results)
    return avg_time, avg_integral


def update_n_points(n_points):
    with open("monte_carlo.fptl", "r") as file:
        lines = file.readlines()

    application_index = next(i for i, line in enumerate(lines) if "Application" in line)
    for i in range(application_index, len(lines)):
        if "N = " in lines[i]:
            lines[i] = f"N = {n_points};\n"
            break

    with open("monte_carlo.fptl", "w") as file:
        file.writelines(lines)


def save_results(results, filename):
    with open(filename, "w") as file:
        file.write("Monte Carlo FPTL Testing Results (Averaged over 5 runs)\n")
        file.write(f"Date and time: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        file.write("Maximum available processes: 20\n")
        file.write("=====================================================================\n\n")
        for n_points, data in results.items():
            file.write(f"Results for {n_points} points (averaged over 5 runs):\n")
            file.write("-" * 120 + "\n")
            file.write("Processes    |        1           |        3            |        6            |       12           |       18            |       20       \n")
            file.write("Time           |     {:.3f}       |     {:.3f}        |     {:.3f}        |     {:.3f}      |     {:.3f}           |     {:.3f}     \n".format(*data['time']))
            file.write("Speedup      |     {:.4f}     |     {:.4f}     |     {:.4f}     |     {:.4f}      |     {:.4f}         |     {:.4f}\n".format(*data['speedup']))
            file.write("Result         |     {:.4f}    |     {:.4f}    |     {:.4f}   |     {:.4f}     |     {:.4f}        |     {:.4f}\n".format(*data['result']))
            file.write("-" * 120 + "\n\n")


def plot_results(results):
    processes = [1, 3, 6, 12, 18, 20]
    for n_points, data in results.items():
        plt.plot(processes, data['time'], label=f'{n_points} points')
    plt.xlabel('Number of Processes')
    plt.ylabel('Time (s)')
    plt.title('Execution Time by Number of Processes')
    plt.legend()
    plt.show()


def main():
    n_points_list = [10000, 100000, 1000000, 10000000]
    processes_list = [1, 3, 6, 12, 18, 20]
    results = {}

    for n_points in n_points_list:
        update_n_points(n_points)
        times = []
        integrals = []
        for n_processes in processes_list:
            avg_time, avg_integral = run_monte_carlo(n_processes)
            times.append(avg_time)
            integrals.append(avg_integral)
        speedups = [times[0] / t for t in times]
        results[n_points] = {'time': times, 'speedup': speedups, 'result': integrals}

    save_results(results, "results.txt")
    plot_results(results)


if __name__ == "__main__":
    main()
