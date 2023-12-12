#include <cuda_runtime.h>
#include <iostream>
#include <vector>
#include <cmath>

// https://www.wikiwand.com/en/Floyd%E2%80%93Warshall_algorithm
extern "C" __global__ void tc_kernel(bool *graph, int n) {
    int k = blockIdx.x;
    int i = threadIdx.y;
    int j = threadIdx.x;

    if (i < n && j < n) {
        __syncthreads(); // gotta sync before readin

        if (graph[i * n + k] && graph[k * n + j]) {
            graph[i * n + j] = true;
        }

        __syncthreads(); // gotta sync after updates
    }
}