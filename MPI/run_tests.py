import subprocess
import re
from datetime import datetime
import matplotlib.pyplot as plt
import os
import logging

logging.basicConfig(
    level=logging.ERROR,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)


def update_input_file(processes, points):
    input_content = f"""function: sin(x1) + 2cos(x2)
variables: 2
bounds: 0 5
points: {points}
count_processes: {processes}
"""
    with open('input.txt', 'w', encoding='utf-8') as f:
        f.write(input_content)


def ensure_plots_dir():
    plots_dir = 'plots'
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)
    return plots_dir


def run_test(processes, points):
    update_input_file(processes, points)
    mpiexec = "C:/Program Files/Microsoft MPI/Bin/mpiexec.exe"

    try:
        result = subprocess.run([mpiexec, '-n', str(processes), 'program.exe'],
                              capture_output=True,
                              text=True,
                              encoding='utf-8')

        result_match = re.search(r'интегрирования:\s*(-?\d+\.\d+)', result.stdout)
        time_match = re.search(r'выполнения:\s*(-?\d+\.\d+)', result.stdout)

        if result_match and time_match:
            return float(result_match.group(1)), float(time_match.group(1))
        else:
            logging.error("Could not parse program output")
            return None, None
    except Exception as e:
        logging.error(f"Error running test: {e}")
        return None, None


def find_best_result(results, correct_result=-6.00755):
    if not results:
        return None
    return min(results, key=lambda x: abs(x[3] - correct_result))


def plot_results(all_results, process_counts, point_counts, timestamp):
    plots_dir = ensure_plots_dir()
    processes_data = {points: [] for points in point_counts}
    speedup_data = {points: [] for points in point_counts}

    for points, results in all_results.items():
        for proc, time, speedup, _ in results:
            processes_data[points].append(time)
            speedup_data[points].append(speedup)

    plt.figure(figsize=(10, 6))
    for points in point_counts:
        plt.plot(process_counts[:len(speedup_data[points])],
                 speedup_data[points],
                 marker='o',
                 label=f'{points:,} points')

    plt.xlabel('Processes')
    plt.ylabel('Speedup')
    plt.title('Speedup by Number of Processes')
    plt.grid(True)
    plt.legend()
    plt.savefig(os.path.join(plots_dir, f'speedup_by_processes_{timestamp}.png'), dpi=300, bbox_inches='tight')
    plt.close()

    plt.figure(figsize=(10, 6))
    selected_processes = process_counts
    for proc_idx, proc_count in enumerate(process_counts):
        if proc_count in selected_processes:
            speedups = [speedup_data[points][proc_idx]
                       for points in point_counts
                       if proc_idx < len(speedup_data[points])]
            if speedups:
                plt.plot(point_counts[:len(speedups)],
                         speedups,
                         marker='o',
                         label=f'{proc_count} processes')

    plt.xscale('log')
    plt.xlabel('Points')
    plt.ylabel('Speedup')
    plt.title('Speedup by Number of Points')
    plt.grid(True)
    plt.legend()
    plt.savefig(os.path.join(plots_dir, f'speedup_by_points_{timestamp}.png'), dpi=300, bbox_inches='tight')
    plt.close()


def format_table_row(data):
    return " | ".join(f"{x:^15}" for x in data)


def run_tests_for_points(points, process_counts, max_processes, output_file):
    results = []
    integral_result, base_time = run_test(1, points)

    if base_time is not None and base_time > 0:
        speedup = 1.0
        results.append((1, base_time, speedup, integral_result))

        for processes in process_counts[1:]:
            if processes > max_processes:
                continue

            integral_result, execution_time = run_test(processes, points)
            if execution_time is not None and execution_time > 0:
                speedup = base_time / execution_time
                results.append((processes, execution_time, speedup, integral_result))

    if results:
        best_result = find_best_result(results)
        output_file.write(f"\nResults for {points:,} points:\n".replace(',', ' '))
        output_file.write("-" * 80 + "\n")
        headers = ["Processes"] + [str(r[0]) for r in results]
        output_file.write(" | ".join(f"{x:^15}" for x in headers) + "\n")
        times = ["Time"] + [f"{r[1]:.4f}" for r in results]
        output_file.write(" | ".join(f"{x:^15}" for x in times) + "\n")
        speedups = ["Speedup"] + [f"{r[2]:.4f}" for r in results]
        output_file.write(" | ".join(f"{x:^15}" for x in speedups) + "\n")
        integral_results = ["Result"] + [f"{r[3]:.4f}" for r in results]
        output_file.write(" | ".join(f"{x:^15}" for x in integral_results) + "\n")
        output_file.write("-" * 80 + "\n")
        output_file.write(
            f"Best approximation to -6.00755: {best_result[3]:.5f} (error: {abs(best_result[3] + 6.00755):.5f})\n")

    return results


def main():
    build_result = subprocess.run(['make', 'build'],
                                capture_output=True,
                                text=True,
                                encoding='utf-8')
    if build_result.returncode != 0:
        logging.error(f"Build failed: {build_result.stderr}")
        return

    max_processes = 20
    process_counts = [1, 3, 6, 12, 18, 20]
    point_counts = [10000, 100000, 1000000, 10000000]
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f'test_results_{timestamp}.txt'
    all_results = {}

    with open(filename, 'w', encoding='utf-8') as f:
        f.write("Monte Carlo MPI Testing Results\n")
        f.write(f"Date and time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Maximum available processes: {max_processes}\n")
        f.write("=" * 80 + "\n")

        for points in point_counts:
            results = run_tests_for_points(points, process_counts, max_processes, f)
            if results:
                all_results[points] = results
    logging.info(f"\nTesting completed. Results saved to: {filename}")

    if all_results:
        plot_results(all_results, process_counts, point_counts, timestamp)
    logging.info(f"Plots saved as speedup_by_threads_{timestamp}.png and speedup_by_points_{timestamp}.png")

    clean_result = subprocess.run(['make', 'clean'],
                                capture_output=True,
                                text=True,
                                encoding='utf-8')
    if clean_result.returncode != 0:
        logging.error(f"Cleanup failed: {clean_result.stderr}")


if __name__ == "__main__":
    main()
