jaccard <- function(cluster, prediction) {
  k_max <- n_distinct(cluster)
  res <- tibble(k = integer(), jaccard = numeric())
  for (k_val in 1:k) {
    m <- as.matrix(table(cluster == k_val, prediction == k_val))
    j <- m[2,2]/(m[2,1] + m[1,2] + m[2,2])
    res <- bind_rows(res, tibble(k = k_val, jaccard = j))
  }
  return(res)
}