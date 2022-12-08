################################################################################
# tuning the random forest hyperparameters 
################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load parameters
k_range <- parameters[["k_range"]] 
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))

# load data
train2016 <- readRDS(file.path(main_results_path, "train2016.RDS"))

select_features <- readRDS(here::here("analysis", "lib", "select_features.RDS"))

################################################################################
# hyperparameter tuning for RF

# the k and dims for the solutions that we will use to rune the random forest parameters
# *these values are based on the real data*
solutions <- tribble(
  ~k, ~dim,
  5,5,
  6,6,
  7,6
)

tuneRF_res_all <- list()
for (index in 1:nrow(solutions)) {
  
  k <- solutions$k[index]
  dims <- solutions$dim[index]
  keep <- k-1
  
  tuning_clusters <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{dims}.RDS")))[[keep]]$partition
  i <- 1
  ntrees <- c(500, 1000, 1500)
  for (n in ntrees) {
    tuneRF_res <- randomForest::tuneRF(
      x = train2016[,select_features],
      y = factor(tuning_clusters),
      mtryStart = 8, 
      ntreeTry = n,
      stepFactor = 1.25,
      improve = 0.005,
      trace = TRUE,
      plot = FALSE,
      doBest = FALSE,
      # parameters for randomdforest
      replace = TRUE
      )
    readr::write_rds(tuneRF_res, file.path(main_results_path, glue::glue("tuneRF_{k}_{n}.RDS")))
    tuneRF_res_all[[i]] <- tuneRF_res
    i <- i+1
  }
  
}
################################################################################
# plot results
i <- 1
ntrees <- c(500,1000,1500)
tuneRF_res_all <- list()
tuneRF_res <- tibble()
for (index in 1:nrow(solutions)) {
  k <- solutions$k[index]
  for (n in ntrees) {
    tuneRF_res_all[[i]] <- readr::read_rds(file.path(main_results_path, glue::glue("tuneRF_{k}_{n}.RDS")))
    tuneRF_res <- bind_rows(
      tuneRF_res,
      as_tibble(tuneRF_res_all[[i]]) %>% mutate(k = k,ntree = n))
    i <- i+1
  }
}
# plot
tuneRF_res %>%
  mutate_at('k', factor, levels = solutions$k) %>%
  mutate_at('ntree', list(~ factor(., levels = as.character(ntrees)))) %>%
  ggplot(aes(x = mtry, y = OOBError, linetype = ntree, colour = k)) +
  geom_line() +
  geom_point(alpha=0.5) +
  labs(x = 'number of features',
       y = 'OOB error') +
  scale_linetype_discrete(name = 'number of\ntrees') +
  scale_colour_discrete(name = 'number of\nclusters') +
  theme_bw()
ggsave(filename = 'rf_mtry_ntree.png',
       path = main_results_path,
       width = plot_width, height = 10, units = 'cm') 

################################################################################
rf_parameters <- solutions %>%
  mutate(
    mtry_opt = case_when(
      k == 5 ~ 6,
      TRUE ~ 5
    ),
    ntree_opt = 1500
  )

readr::write_rds(rf_parameters, file.path(main_results_path, "rf_parameters.RDS"))
