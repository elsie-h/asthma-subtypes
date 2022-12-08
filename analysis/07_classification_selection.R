################################################################################
# fit classification models to select final cluster solution

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
rf_parameters <- rf_parameters %>%
  mutate(
    # all dimensions that gave stable solutions for the given k
    # *based on the real data*
    dim_vals = case_when(
      k==5 ~ list(4L:5L),
      k==6 ~ list(6L:14L),
      k==7 ~ list(6L)
    )
  )
readr::write_rds(rf_parameters, file.path(main_results_path, "rf_parameters.RDS"))

# load functions
source(here::here("analysis", "functions", "my_clusterboot.R"))
source(here::here("analysis", "functions", "confusion_plot.R"))
source(here::here("analysis", "functions", "jaccard.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

################################################################################
# order cluster solutions (these will be manually reordered later to match RF predictions)
# this is necessary, because applying kmeans to the test dataset might identify the same 
# partition that is predicted using the randon forest, but the clusters may be labelled 
# differently (e.g. kmeans cluster=1 while random forest prediction = 3 etc.)
orders <- vector(mode = 'list', length = 14)
for (index in 1:nrow(rf_parameters)) {
  k  <- rf_parameters$k[index]
  for (dim in rf_parameters$dim_vals[[index]]) {
    orders[[dim]][[k]] <- list(1:k)
  }
}


################################################################################
# train random forests
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  keep <- k-1
  
  for (dim in rf_parameters$dim_vals[[index]]) {
    
    # read clusters from training data
    clusters_rf <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{dim}.RDS")))[[keep]]$partition
    data_in <- train2016 %>% select(all_of(select_features)) %>% mutate(cluster = factor(clusters_rf, levels = as.character(1:k)))
    
    print(glue::glue("----fitting random forest for k={k} and dim={dim}----"))
    rf_res <- randomForest::randomForest(
      formula = cluster ~ .,
      data = data_in,
      importance = TRUE,
      mtry = rf_parameters$mtry_opt[[index]],
      ntree = rf_parameters$ntree_opt[[index]],
      replace=TRUE
      )
    
    filename <- str_c('rf_res', dim, k, sep='_')
    readr::write_rds(rf_res, file.path(main_results_path, glue::glue("{filename}.RDS")))
    
  }
}

################################################################################
# derive clusters in test data
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  
  for (dim in rf_parameters$dim_vals[[index]]) {
    
    print(glue::glue("----clustering for k={k} and dim={dim}----"))
    cb_res <- my_clusterboot(scores = scores_test2016, dims = dim, k_val = k)
    
    filename <- str_c('cb_res_test_2016', dim, k, sep='_')
    readr::write_rds(cb_res, file.path(main_results_path, glue::glue("{filename}.RDS")))
    
  }
}

# predict clusters
preds <- list()
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  preds[[k]] <- NA_character_
  
  for (dim in rf_parameters$dim_vals[[index]]) {
    
    # read random forest
    filename <- str_c('rf_res', dim, k, sep='_')
    rf_res <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
    
    # predict
    preds[[k]][[dim]] <- list(
      randomForest:::predict.randomForest(
        rf_res, newdata = test2016 %>% select(all_of(select_features))
        )
      )
  }
}
readr::write_rds(preds, file.path(main_results_path, "test_preds_list.RDS"))

################################################################################
# just for reordering
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  keep <- k-1
  
  for (dim in rf_parameters$dim_vals[[index]]) {
    
    filename <- str_c('cb_res_test_2016', dim, k, sep='_')
    cb_res <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
    
    p <- test2016 %>%
      select(all_of(select_features)) %>% 
      mutate(cluster = factor(cb_res$partition,
                              levels = as.character(1:k),
                              labels = as.character(orders[[dim]][[k]][[1]]))) %>%
      mutate_at('cluster', list(~ factor(as.numeric(as.character(.))))) %>%
      mutate(predictions = preds[[k]][[dim]][[1]]) %>%
      confusion_plot(yvar = predictions, xvar = cluster,
                     ylab = 'predicted cluster (random forest)',
                     xlab = 'true cluster (k-means)',
                     title = str_c('2016 test dataset (k = ', k, ', dims = ', dim, ')'),
                     accuracy = TRUE)
    print(p)
  }
}

# based on these plots, manually update the orders to maximise the number of
# samples for which "predicted cluster" = "true cluster"
# *the orders below are based on the true data - I won't bother doing this for the dummy data*
# orders[[4]][[5]] <- list(c(3:5,1:2))
# orders[[5]][[5]] <- list(c(4,1,2,3,5))
# orders[[6]][[6]] <- list(c(5,3,4,6,2,1))
# orders[[6]][[7]] <- list(c(6,2,3,5,7,1,4))
# orders[[7]][[6]] <- list(c(2,6,5,1,4,3))
# orders[[8]][[6]] <- list(c(6,3,1,2,4,5))
# orders[[9]][[6]] <- list(c(2,6,5,1,3,4))
# orders[[10]][[6]] <- list(c(6,3,1,2,4,5))
# orders[[11]][[6]] <- list(c(5,3,1,4,2,6))
# orders[[12]][[6]] <- list(c(6,3,1,2,4,5))
# orders[[13]][[6]] <- list(c(2,1,6,4,3,5))
# orders[[14]][[6]] <- list(c(3,5,1,6,2,4))

readr::write_rds(orders, file.path(main_results_path, "orders.RDS"))

################################################################################
# tables of Jaccard coefficients for clusterboot
ncols <- max(rf_parameters$k)+1
rnows <- sum(sapply(unlist(orders, recursive = FALSE), function(x) !is.null(x)))
mat_test <- matrix(
  ncol=ncols,
  nrow=rnows,
  dimnames = list(NULL, c('Dimensions', as.character(1:max(rf_parameters$k))))
  )

i <- 1
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  keep <- k-1
  
  for (dim in rf_parameters$dim_vals[[index]]) {
    
    filename <- str_c('cb_res_test_2016', dim, k, sep='_')
    cb_res_test <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
    jaccards <- rep(NA_real_, max(rf_parameters$k))
    jaccards[1:k] <- round(cb_res_test$subsetmean, 2)
    mat_test[i,] <- c(dim, jaccards)
    i <- i+1
    
  }
}

# print Jaccard coefficients for each solution
as_tibble(mat_test) %>%
  arrange(Dimensions) %>%
  mutate_at('Dimensions', as.integer)

################################################################################
# calculate and plot Jaccard coefficients between predictions and clusters
jaccards_res <- list()
i <- 1
for (index in 1:nrow(rf_parameters)) {
  
  k  <- rf_parameters$k[index]
  keep <- k-1
  
  for (dim in rf_parameters$dim_vals[[index]]) {

    filename <- str_c('cb_res_test_2016', dim, k, sep='_')
    cb_res <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
    
    clusters <- k
    true_clusters <- 
      as.integer(
        as.character(
          factor(
            cb_res$partition,
            levels = as.character(1:k),
            labels = as.character(orders[[dim]][[k]][[1]])
            )
          )
        )
    predicted_clusters <- preds[[k]][[dim]][[1]]
      
    jaccard_tmp <- mltest::ml_test(true = true_clusters, predicted = predicted_clusters)$Jaccard
    jaccards_res[[i]] <- tibble(k = seq_along(jaccard_tmp), jaccard = jaccard_tmp) %>%
      mutate(mca_dims = dim, clusters = clusters)
    i <- i+1
  }
}

bind_rows(jaccards_res) %>%
  group_by(mca_dims, clusters) %>%
  mutate(mean = mean(jaccard)) %>%
  mutate_at(vars(c('clusters','mca_dims')), factor) %>%
  ggplot() +
  geom_point(aes(x = mca_dims, y = jaccard, colour = clusters), alpha = 0.5) +
  geom_point(aes(x = mca_dims, y = mean, colour = clusters), alpha = 1, size = 3) +
  labs(
    x = 'MCA dimensions', 
    y = 'Jaccard coefficient',
    caption = str_wrap(width = caption_width, 'Small points show the per-cluster Jaccard coefficient, large points show the mean Jaccard coefficient over all clusters. Results from the 2016 test set.')
    ) +
  theme_elsie()
ggsave(filename = "classification_all_jaccards.png",
       path = main_results_path,
       height = 10,
       width = plot_width,
       units = 'cm')  

# based on these results select the final values of clusters (k) and mca dimensions (dim)
# *these values are based on the real data*
final_parameters <- list(
  dim = 12,
  k = 6
)
readr::write_rds(final_parameters, here::here(main_results_path, "final_parameters.RDS"))
