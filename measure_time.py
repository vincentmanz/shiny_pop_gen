import time
import subprocess

def measure_execution_time(script_name, is_r_script=False):
    start_time = time.time()
    
    print(f"Testing execution time for {script_name}...")
    
    try:
        # Execute the script using subprocess
        if is_r_script:
            subprocess.run(["Rscript", script_name], check=True)
        else:
            subprocess.run(["python", script_name], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error occurred while running {script_name}: {e}")
        return None
    
    end_time = time.time()
    execution_time = end_time - start_time
    return execution_time

# Test the execution time of LD_parallel.py
parallel_time = measure_execution_time("LD_parallel.py")
if parallel_time is not None:
    print(f"Execution time for LD_parallel.py: {parallel_time:.2f} seconds")

# Test the execution time of LD_sequencial.py
sequencial_time = measure_execution_time("LD_sequencial.py")
if sequencial_time is not None:
    print(f"Execution time for LD_sequencial.py: {sequencial_time:.2f} seconds")

# Test the execution time of LD.R
r_script_time = measure_execution_time("LD.R", is_r_script=True)
if r_script_time is not None:
    print(f"Execution time for LD.R: {r_script_time:.2f} seconds")

# Test the execution time of LD_parallel_optimized.py
optimized_parallel_time = measure_execution_time("LD_parallel_optimized.py")
if optimized_parallel_time is not None:
    print(f"Execution time for LD_parallel_optimized.py: {optimized_parallel_time:.2f} seconds")

# Compare times
if parallel_time is not None and sequencial_time is not None:
    speedup_parallel = sequencial_time / parallel_time if parallel_time > 0 else None
    if speedup_parallel is not None:
        print(f"Speedup achieved with LD_parallel.py: {speedup_parallel:.2f}x")

if optimized_parallel_time is not None and sequencial_time is not None:
    speedup_optimized = sequencial_time / optimized_parallel_time if optimized_parallel_time > 0 else None
    if speedup_optimized is not None:
        print(f"Speedup achieved with LD_parallel_optimized.py: {speedup_optimized:.2f}x")
