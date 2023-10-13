# Load the required GPU libraries
library(gpuR)

# Convert your data to a numeric matrix (assuming filtered_data is a data frame)
gpu_data <- as.matrix(filtered_data[, 6:11])

gpu_data <- matrix(as.numeric(unlist(strsplit(gpu_data, "/"))), ncol = 2, byrow = TRUE)

# Allocate a GPU matrix and transfer data
data_gpu <- gpuMatrix(gpu_data)

# Define a GPU function for shuffling pairs of values
shuffle_gpu <- gpuKernel(code = "
__global__ void shuffle_GPU(gpuMatrix data_gpu, int n) {
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int i = tid;  // Index for the row

  if (i < n) {
    int j = i % 6;  // Index to shuffle pairs (0 to 5)

    // Extract and shuffle the pair of values
    int tmp = data_gpu[i + j * n];
    data_gpu[i + j * n] = data_gpu[i + ((j + 1) % 6) * n];
    data_gpu[i + ((j + 1) % 6) * n] = tmp;
  }
}
", options = list("nV", nrow(data_gpu)))

# Note: This code defines a CUDA kernel, so the syntax is different from regular R code.
# It uses C-like syntax for GPU parallel processing.

# To shuffle data on the GPU using the kernel:
shuffle_gpu(data_gpu, nrow(data_gpu))


# Number of GPU cores to use
num_gpu_cores <- nrow(gpu_data)

# Set the number of repetitions
n_rep <- 100

# Create storage for results
result_FST <- matrix(0, n_rep, n_rep)
result_FIS <- matrix(0, n_rep, n_rep)

# Perform parallel computations on the GPU
for (i in 1:n_rep) {
  for (j in 1:n_rep) {
    # Shuffle the data on the GPU
    shuffle_gpu(data_gpu, threads = num_gpu_cores)
    
    # Copy the shuffled data back to the CPU
    shuffled_data <- as.matrix(data_gpu)
    
    # Compute the statistics on the shuffled data (you'll need GPU-compatible functions)
    # Replace the code below with GPU-compatible FST and FIS computations
    wc_result_FST <- 0
    wc_result_FIS <- 0
    
    # Store the FST and FIS results
    result_FST[i, j] <- wc_result_FST
    result_FIS[i, j] <- wc_result_FIS
  }
}

# Combine the results into data frames (if needed)
result_FST_df <- as.data.frame(result_FST)
result_FIS_df <- as.data.frame(result_FIS)

# Print or use the results as needed
print(result_FST_df)
print(result_FIS_df)
