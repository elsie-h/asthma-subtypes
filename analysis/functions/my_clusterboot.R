# define function for clustering test samples
my_clusterboot <- function(scores, dims, k_val, subset_size) {
  clusterboot_res <- fpc::clusterboot(
    data = scores[,1:dims],
    B=100,
    bootmethod = 'subset',
    subtuning = subset_size,
    clustermethod = fpc::kmeansCBI,
    k = k_val,
    runs = 25,
    iter.max = 25,
    dissolution = 0.5,
    recover = 0.75,
    seed = seed)
}
