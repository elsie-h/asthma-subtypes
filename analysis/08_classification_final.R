################################################################################
# select final cluster solution

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load data
train2016 <- readr::read_rds(file.path(main_results_path, "train2016.RDS"))
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))

scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))
scores_test2016 <- readr::read_rds(file.path(main_results_path, "scores_test2016.RDS"))

# load parameters
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))
rf_parameters <- readr::read_rds(file.path(main_results_path, "rf_parameters.RDS"))
final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_dims <- final_parameters[["dim"]]
final_k <- final_parameters[["k"]]
final_keep <- final_k-1
order_train <- 1:final_k
orders_final <- list()
orders_final[["2016train"]] <- order_train

# load functions
source(here::here("analysis", "functions", "my_clusterboot.R"))
source(here::here("analysis", "functions", "confusion_plot.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

################################################################################
# define subtype labels
subtype_labels <- c(
  "1. low ICS use and low healthcare utilisation",
  "2. low-to-medium ICS use",
  "3. low-to-medium ICS use and comorbidities",
  "4. varied ICS use and comorbid COPD",
  "5. high ICS use",
  "6. very high ICS use"
)
readr::write_rds(subtype_labels, file.path(main_results_path, "subtype_labels.RDS"))

################################################################################
# final clusters in 2016 train
clusterboot_res <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{final_dims}.RDS")))[[final_keep]]
# reorder
clusters_final <- 
  factor(
    as.character(
      factor(
        as.numeric(clusterboot_res$partition),
        levels = 1:final_k,
        labels = as.character(order_train)
        )
      )
    )

# final clusters in 2016 test
filename <- str_c('cb_res_test_2016', final_dims, final_k, sep='_')
cb_res_test_final <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))

################################################################################
# train random forest model
data_in <- train2016 %>%
  select(all_of(select_features)) %>% 
  mutate(
    cluster = factor(
      clusterboot_res$partition,
      levels = as.character(1:final_k),
      labels = as.character(order_train)
      )
    ) %>%
  mutate_at('cluster', list(~ factor(as.numeric(as.character(.)))))

rf_res_final <- randomForest::randomForest(
  formula = cluster ~ .,
  data = data_in,
  importance = TRUE,
  mtry = rf_parameters$mtry_opt[rf_parameters$k == final_k],
  ntree = rf_parameters$ntree_opt[rf_parameters$k == final_k],
  replace=TRUE
  )

readr::write_rds(rf_res_final, file.path(main_results_path, "rf_final.RDS"))

################################################################################
# final predictions in 2016 train and test
pred2016_train <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = train2016 %>% select(all_of(select_features))
  )
readr::write_rds(pred2016_train, file.path(main_results_path, "pred2016_train.RDS"))

pred2016 <- randomForest:::predict.randomForest(
  rf_res_final, 
  newdata = test2016 %>% select(all_of(select_features))
  )
readr::write_rds(pred2016, file.path(main_results_path, "pred2016.RDS"))

################################################################################
# plot 2016 test confusion matrix 
order_2016 <-  1:final_k
orders_final[["2016test"]] <- order_2016
readr::write_rds(orders_final, file.path(main_results_path, "orders_final.RDS"))

test2016 %>%
  select(all_of(select_features)) %>% 
  mutate(
    cluster = factor(
      cb_res_test_final$partition,
      levels = as.character(1:final_k),
      labels = as.character(order_2016))
    ) %>%
  mutate_at('cluster', list(~ factor(as.numeric(as.character(.))))) %>%
  mutate(
    predictions = factor(
      pred2016,
      labels = str_wrap(subtype_labels, 16)
      )
    ) %>%
  confusion_plot(
    yvar = predictions, xvar = cluster,
    ylab = 'predicted subtype (random forest)',
    xlab = 'true cluster (k-means)',
    title = '2016 test dataset',
    accuracy = TRUE
    )
ggsave(filename = 'rf_test2016.png',
       path = main_results_path,
       width = plot_width, height = 8, units = 'cm')

################################################################################
mca_eig <- readr::read_rds(file.path(main_results_path, 'mca_eig.RDS'))

# plot scores
scores_train2016 %>%
  select(1:final_dims) %>%
  pivot_longer(cols = everything()) %>%
  mutate_at(
    'name', 
    list(
      ~ factor(
        str_remove(., 'Dim'),
        levels = as.character(1:final_dims),
        labels = str_c(as.character(1:final_dims), ' (', round(unname(mca_eig[1:max(final_dims),'percentage of variance']),1), '%)')
        )
      )
    ) %>%
  ggplot(aes(x = value, colour = name)) +
  geom_freqpoly(binwidth = 0.1) +
  scale_color_discrete(name = 'MCA dimension (*)') +
  labs(
    x = 'score',
    caption = '*Percent of variance explained by each MCA dimension given in brackets.'
    ) +
  theme_elsie()
ggsave(filename = glue::glue("mca_scores_{final_dims}.png"),
       path = main_results_path,
       width = plot_width, height = 12, units = 'cm')
