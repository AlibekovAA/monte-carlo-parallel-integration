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


def update_input_file(threads, points):
    input_content = f"""bounds: 0 5, 0 5
points: {points}
count_processes: {threads}
"""
    with open('input.txt', 'w', encoding='utf-8') as f:
        f.write(input_content)


def ensure_plots_dir():
    plots_dir = 'plots'
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)
    return plots_dir


def run_test(threads, points):
    update_input_file(threads, points)

    try:
        result = subprocess.run(['make', 'run'],
                                capture_output=True,
                                text=True,
                                encoding='utf-8')

        result_match = re.search(r'Integral result:\s*(-?\d+\.\d+)', result.stdout)
        time_match = re.search(r'Time taken:\s*(-?\d+\.\d+)', result.stdout)

        if result_match and time_match:
            integral_result = float(result_match.group(1))
            execution_time = float(time_match.group(1))
            return integral_result, execution_time
        else:
            if not result_match:
                logging.error("Could not find integral result in output")
            if not time_match:
                logging.error("Could not find execution time in output")
            return None, None
    except subprocess.CalledProcessError as e:
        logging.error(f"Error running test: {e}")
        return None, None
    except Exception as e:
        logging.error(f"Unexpected error: {e}")
        return None, None


def find_best_result(results, correct_result=-6.00755):
    if not results:
        return None
    return min(results, key=lambda x: abs(x[3] - correct_result))


def plot_results(all_results, thread_counts, point_counts, timestamp):
    plots_dir = ensure_plots_dir()
    threads_data = {points: [] for points in point_counts}
    speedup_data = {points: [] for points in point_counts}

    for points, results in all_results.items():
        for thread, time, speedup, _ in results:
            threads_data[points].append(time)
            speedup_data[points].append(speedup)

    plt.figure(figsize=(10, 6))
    for points in point_counts:
        plt.plot(thread_counts[:len(speedup_data[points])],
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
    selected_threads = [1, 3, 6, 12, 18, 20]
    for thread_idx, thread_count in enumerate(thread_counts):
        if thread_count in selected_threads:
            speedups = [speedup_data[points][thread_idx]
                        for points in point_counts
                        if thread_idx < len(speedup_data[points])]
            if speedups:
                plt.plot(point_counts[:len(speedups)],
                         speedups,
                         marker='o',
                         label=f'{thread_count} processes')

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


def run_test_multiple_times(threads, points, num_runs=5):
    total_integral = 0.0
    total_time = 0.0
    successful_runs = 0

    for _ in range(num_runs):
        integral_result, execution_time = run_test(threads, points)
        if integral_result is not None and execution_time is not None:
            total_integral += integral_result
            total_time += execution_time
            successful_runs += 1

    if successful_runs > 0:
        return (total_integral / successful_runs,
                total_time / successful_runs)
    return None, None


def run_tests_for_points(points, thread_counts, output_file):
    results = []
    integral_result, base_time = run_test_multiple_times(1, points)

    if base_time is not None and base_time > 0:
        speedup = 1.0
        results.append((1, base_time, speedup, integral_result))

        for threads in thread_counts[1:]:
            integral_result, execution_time = run_test_multiple_times(threads, points)

            if execution_time is not None and execution_time > 0:
                speedup = base_time / execution_time
                results.append((threads, execution_time, speedup, integral_result))

    if results:
        best_result = find_best_result(results)

        output_file.write(f"\nResults for {points:,} points (averaged over 5 runs):\n".replace(',', ' '))
        output_file.write("-" * 80 + "\n")

        headers = ["Threads"] + [str(r[0]) for r in results]
        output_file.write(format_table_row(headers) + "\n")

        times = ["Time"] + [f"{r[1]:.4f}" for r in results]
        output_file.write(format_table_row(times) + "\n")

        speedups = ["Speedup"] + [f"{r[2]:.4f}" for r in results]
        output_file.write(format_table_row(speedups) + "\n")

        integral_results = ["Result"] + [f"{r[3]:.4f}" for r in results]
        output_file.write(format_table_row(integral_results) + "\n")

        output_file.write("-" * 80 + "\n")
        output_file.write(
            f"Best approximation to -6.00755: {best_result[3]:.5f} (error: {abs(best_result[3] + 6.00755):.5f})\n")

    return results


def main():
    thread_counts = [1, 3, 6, 12, 18, 20]
    point_counts = [10000, 100000, 1000000, 10000000, 100000000]

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f'test_results_{timestamp}_avg5.txt'
    all_results = {}

    with open(filename, 'w', encoding='utf-8') as f:
        f.write("Monte Carlo Erlang Testing Results (Averaged over 5 runs)\n")
        f.write(f"Date and time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write("=" * 80 + "\n")

        for points in point_counts:
            results = run_tests_for_points(points, thread_counts, f)
            if results:
                all_results[points] = results

    plot_results(all_results, thread_counts, point_counts, timestamp)
    subprocess.run(['make', 'clean'],
                   capture_output=True,
                   text=True,
                   encoding='utf-8')


if __name__ == "__main__":
    main()
