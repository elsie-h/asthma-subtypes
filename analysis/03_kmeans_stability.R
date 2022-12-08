################################################################################
# cluster analysis
################################################################################

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load data
scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))

# define parameters
k_range <- parameters[["k_range"]] 
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))

################################################################################
# run stability analysis
# load clusterboot function
source(here::here("analysis", "functions", "my_clusterboot.R"))

for (dim in dims_range) {
  
  print(glue::glue("start clusterboot for {dim} dimensions."))
  
  # filename for results
  filename <- glue::glue("train_clusterboot_res_{dim}")
  
  # clusterboot
  clusterboot_res <- lapply(
    k_range, 
    function(x)
      my_clusterboot(
        scores = scores_train2016,
        dims = dim,
        k_val = x,
        subset_size = parameters[["subset_size"]]
      )
  )
  
  # save results
  readr::write_rds(clusterboot_res, file.path(main_results_path, glue::glue("{filename}.RDS")))
  
}

# tabulate clusterboot Jaccard coefficients
for (dim in dims_range) {
  
  clusterboot_res <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{dim}.RDS")))
  cat("\n")
  cat(glue::glue("Jaccard coefficients for {min(k_range)} to {max(k_range)} clusters when MCA dimensions = {dim}:"))
  cat("\n")
  
  bind_rows(
    lapply(
      seq_along(clusterboot_res), # for each value of k in k_range
      function(i)
        tibble(
          # extract Jaccard coefficients
          Jaccard = round(clusterboot_res[[i]]$subsetmean, 2), k = i+1 
        ) %>%
        mutate(cluster = row_number())
    )
  ) %>%
    # reshape to max(k_range) x max(k_range) matrix and print
    pivot_wider(names_from = 'cluster', values_from = 'Jaccard') %>%
    mutate_at('k', as.integer) %>%
    print(n = max(k_range))
  
}
