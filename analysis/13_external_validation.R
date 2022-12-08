################################################################################
#  external validation

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# read the cleaned dataset
validation2016 <- readr::read_rds(file.path(validation_results_path, "validation2016.RDS"))
validation2017 <- readr::read_rds(file.path(validation_results_path, "validation2017.RDS"))
validation2018 <- readr::read_rds(file.path(validation_results_path, "validation2018.RDS"))

# final number of clusters and dimensions from OPCRD results
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))
rf_parameters <- readr::read_rds(file.path(main_results_path, "rf_parameters.RDS"))
final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_dims <- final_parameters[["dim"]]
final_k <- final_parameters[["k"]]

# load functions
source(here::here("analysis", "functions", "my_clusterboot.R"))
source(here::here("analysis", "functions", "confusion_plot.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

# load random forest model
rf_res_final <- readr::read_rds(file.path(main_results_path, "rf_final.RDS"))

################################################################################
# apply MCA and calculate scores

scores_function <- function(data, dims = final_dims) {
  
  mca_res <- FactoMineR::MCA(data[, select_features], ncp = dims, graph = FALSE)
  
  as_tibble(mca_res$ind$coord) %>%
    rename_all(list(~ str_remove(., '\\s')))
  
}

scores_validation2016 <- scores_function(validation2016)
readr::write_rds(scores_validation2016, file.path(validation_results_path, "scores_validation2016.RDS"), compress = "gz")

scores_validation2017 <- scores_function(validation2017)
readr::write_rds(scores_validation2017, file.path(validation_results_path, "scores_validation2017.RDS"), compress = "gz")

scores_validation2018 <- scores_function(validation2018)
readr::write_rds(scores_validation2018, file.path(validation_results_path, "scores_validation2018.RDS"), compress = "gz")

################################################################################
# apply cluster analysis

clusters_2016 <- my_clusterboot(
  scores = scores_validation2016, 
  dims = final_dims,
  k_val = final_k,
  subset_size = parameters[["subset_size"]]
  )
readr::write_rds(clusters_2016, file.path(validation_results_path, "clusters_2016.RDS"))

clusters_2017 <- my_clusterboot(
  scores = scores_validation2017, 
  dims = final_dims,
  k_val = final_k,
  subset_size = parameters[["subset_size"]]
)
readr::write_rds(clusters_2017, file.path(validation_results_path, "clusters_2017.RDS"))

clusters_2018 <- my_clusterboot(
  scores = scores_validation2018, 
  dims = final_dims,
  k_val = final_k,
  subset_size = parameters[["subset_size"]]
)
readr::write_rds(clusters_2018, file.path(validation_results_path, "clusters_2018.RDS"))


################################################################################
# predict subtypes 
pred2016 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = validation2016 %>% select(all_of(select_features))
  )
readr::write_rds(pred2016, file.path(validation_results_path, "pred2016.RDS"))

pred2017 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = validation2017 %>% select(all_of(select_features))
  )
readr::write_rds(pred2017, file.path(validation_results_path, "pred2017.RDS"))

pred2018 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = validation2018 %>% select(all_of(select_features))
  )
readr::write_rds(pred2018, file.path(validation_results_path, "pred2018.RDS"))

################################################################################
# set orders before viewing plots
order_2016 <- order_2017 <- order_2018 <- 1:final_k

################################################################################
# create confusion plots
validation_confusion_plots(
  validation = "external",
  data_2016 = validation2016 %>% mutate(cluster = clusters_2016$partition, predictions = pred2016),
  data_2017 = validation2017 %>% mutate(cluster = clusters_2017$partition, predictions = pred2017),
  data_2018 = validation2018 %>% mutate(cluster = clusters_2018$partition, predictions = pred2018)
)

# reorder the clusters
validation_confusion_plots(
  validation = "external",
  data_2016 = validation2016 %>% mutate(cluster = clusters_2016$partition, predictions = pred2016),
  data_2017 = validation2017 %>% mutate(cluster = clusters_2017$partition, predictions = pred2017),
  data_2018 = validation2018 %>% mutate(cluster = clusters_2018$partition, predictions = pred2018),
  order_2016 = c(6,1,5,2,4,3),
  order_2017 = c(6,3,5,1,4,2),
  order_2018 = c(1,5,6,2,4,3)
)

ggsave(filename = 'external_validation_confusion_plots.pdf',
       path = validation_results_path,
       width = plot_width, height = 18, units = 'cm')
