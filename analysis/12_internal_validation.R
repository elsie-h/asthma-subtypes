################################################################################
#  internal validation

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load data
train2016 <- readr::read_rds(file.path(main_results_path, "train2016.RDS"))
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))
test2017 <- readr::read_rds(file.path(main_results_path, "test2017.RDS"))
test2018 <- readr::read_rds(file.path(main_results_path, "test2018.RDS"))

scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))
scores_test2016 <- readr::read_rds(file.path(main_results_path, "scores_test2016.RDS"))
scores_test2017 <- readr::read_rds(file.path(main_results_path, "scores_test2017.RDS"))
scores_test2018 <- readr::read_rds(file.path(main_results_path, "scores_test2018.RDS"))

# load parameters
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))
rf_parameters <- readr::read_rds(file.path(main_results_path, "rf_parameters.RDS"))
final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_dims <- final_parameters[["dim"]]
final_k <- final_parameters[["k"]]

# load random forest model
rf_res_final <- readr::read_rds(file.path(main_results_path, "rf_final.RDS"))

# load final clusters
cb_res_test_final <- readr::read_rds(file.path(main_results_path, glue::glue("cb_res_test_2016_{final_dims}_{final_k}.RDS")))
pred2016 <- readr::read_rds(file.path(main_results_path, "pred2016.RDS"))
orders_final <- readr::read_rds(file.path(main_results_path, "orders_final.RDS"))

# load functions
source(here::here("analysis", "functions", "my_clusterboot.R"))
source(here::here("analysis", "functions", "confusion_plot.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

################################################################################
# 2017
clusters2017 <- my_clusterboot(
  scores = scores_test2017, 
  dims = final_dims, 
  k_val = final_k, 
  subset_size = parameters[["subset_size"]]
  )
filename_2017 <- str_c('cb_res_test_2017', final_dims, final_k, sep='_')
readr::write_rds(clusters2017, file.path(main_results_path, glue::glue("{filename_2017}.RDS")))

# 2018
clusters2018 <- my_clusterboot(
  scores = scores_test2018, 
  dims = final_dims, 
  k_val = final_k, 
  subset_size = parameters[["subset_size"]]
)
filename_2018 <- str_c('cb_res_test_2018', final_dims, final_k, sep='_')
readr::write_rds(clusters2018, file.path(main_results_path, glue::glue("{filename_2018}.RDS")))

################################################################################
# random forest predictions
pred2017 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = test2017 %>% select(all_of(select_features))
)
readr::write_rds(pred2017, file.path(main_results_path, "pred2017.RDS"))

pred2018 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = test2018 %>% select(all_of(select_features))
)
readr::write_rds(pred2018, file.path(main_results_path, "pred2018.RDS"))

################################################################################
# create internal validation plots
validation_confusion_plots(
  data_2016 = test2016 %>% mutate(cluster = cb_res_test_final$partition, predictions = pred2016),
  data_2017 = test2017 %>% mutate(cluster = clusters2017$partition, predictions = pred2017),
  data_2018 = test2018 %>% mutate(cluster = clusters2018$partition, predictions = pred2018)
)

################################################################################
# update cluster order based on plot
# create final plot with new order and save
# *the orders below are based on the real data*
orders_final[["2017"]] <- c(1,6,5,4,2,3)
orders_final[["2018"]] <- c(4,6,5,2,3,1)


validation_confusion_plots(
  data_2016 = test2016 %>% mutate(cluster = cb_res_test_final$partition, predictions = pred2016),
  data_2017 = test2017 %>% mutate(cluster = clusters2017$partition, predictions = pred2017),
  data_2018 = test2018 %>% mutate(cluster = clusters2018$partition, predictions = pred2018),
  order_2016 = orders_final[["2016test"]],
  order_2017 = orders_final[["2017"]],
  order_2018 = orders_final[["2018"]]
)

ggsave(filename = 'internal_validation_confusion_plots.pdf',
       path = main_results_path,
       width = plot_width, height = 18, units = 'cm')

################################################################################
# plot clusterboot Jaccards
tibble(
  cluster = factor(c(orders_final[["2017"]], orders_final[["2018"]])),
  year = factor(rep(c(2017, 2018), each = final_k)),
  jaccard = c(clusters2017$subsetmean, clusters2018$subsetmean)
  ) %>%
  ggplot(aes(x = cluster, y = jaccard, fill = year)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_hline(yintercept = 0.85, linetype = 'dashed') +
  labs(y = 'average Jaccard coefficient',
       title = str_c('Cluster stability in OPCRD validation datasets'),
       caption = str_wrap(width = caption_width, 'Method: kmeans with 25 random starts and a maximum of 25 iterations. Stability assessment: 100 samples, each of size 10,000')) +
  theme_elsie()
ggsave(filename = 'internal_validation_jaccard.png',
       path = main_results_path,
       width = plot_width, height = 10, units = 'cm')

