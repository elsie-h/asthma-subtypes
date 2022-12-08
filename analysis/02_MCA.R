################################################################################
# MCA
################################################################################

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

train2016 <- readr::read_rds(file.path(main_results_path, "train2016.RDS"))
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))
test2017 <- readr::read_rds(file.path(main_results_path, "test2017.RDS"))
test2018 <- readr::read_rds(file.path(main_results_path, "test2018.RDS"))

################################################################################
# prepare nonovelapping subsamples of 10,000 (without replacement)
set.seed(seed)
X_list <- list()
ids <- 1:nrow(train2016)
length_list <- 4
for (i in 1:length_list) {
  samples <- sample(ids, size = parameters[["subset_size"]])
  X_list[[i]] <- train2016[samples, select_features]
  ids <- ids[!(ids %in% samples)]
}

################################################################################
# apply MCA to each of the subsets
mca_list <- lapply(
  X_list, 
  function(x) 
    FactoMineR::MCA(x[,select_features], ncp = length(select_features), graph = FALSE)
  )
# apply MCA to the full sample
mca_full <- FactoMineR::MCA(
  train2016[, select_features], 
  ncp = length(select_features), 
  graph = FALSE
  )

# list of all MCA results
mca_list[[5]] <- mca_full

################################################################################
source(here::here("analysis", "functions", "mca_plot.R"))
source(here::here("analysis", "functions", "correlation_ratio_plot.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

mca_plot(mca_list = mca_list, 
         dim_lines = NA_integer_,
         title = NULL)

# based on the plot from the real data the following dims_range was set 
# (the area in which the elbow of the plot can be identified)
# *conclusions fromn the dummy data may be different*
dims_range <- 4:14
readr::write_rds(dims_range, file.path(main_results_path, "dims_range.RDS"))

mca_plot(mca_list = mca_list, 
         dim_lines = range(dims_range),
         title = NULL)

ggsave(filename = "scree_plot.png",
       path = main_results_path,
       width = plot_width, height = 12, units = 'cm')

# specify number of dimensions to plot
plot_dims <- max(dims_range)

# save table with cumulative percent of variances
readr::write_rds(mca_list[[5]]$eig, file.path(main_results_path, 'mca_eig.RDS'))

################################################################################
# plot correlation ratios
correlation_ratio_plot(
  mca_res  = mca_full,
  dims = nrow(mca_full$eig),
  dim_final = plot_dims,
  title = NULL
  ) 
ggsave(filename = "correlation_ratio_plot.png",
       path = main_results_path,
       width = plot_width, height = 21, units = 'cm')

################################################################################
# calculate scores in all datasets and save
scores_function <- function(data, maxdims = plot_dims) {
  mca_res <-FactoMineR::MCA(
    data[, select_features], 
    ncp = plot_dims, 
    graph = FALSE
    )
  as_tibble(mca_res$ind$coord) %>%
    rename_all(~ str_remove(., '\\s'))
}

scores_train2016 <- as_tibble(mca_full$ind$coord[,1:plot_dims]) %>%
  rename_all(~ str_remove(., '\\s'))
readr::write_rds(scores_train2016, file.path(main_results_path, 'scores_train2016.RDS'))

scores_test2016 <- scores_function(data = test2016)
readr::write_rds(scores_test2016, file.path(main_results_path, 'scores_test2016.RDS'))

scores_test2017 <- scores_function(data = test2017)
readr::write_rds(scores_test2017, file.path(main_results_path, 'scores_test2017.RDS'))

scores_test2018 <- scores_function(data = test2018)
readr::write_rds(scores_test2018, file.path(main_results_path, 'scores_test2018.RDS'))
  
################################################################################
# plot scores
scores_train2016 %>%
  select(1:plot_dims) %>%
  pivot_longer(cols = everything()) %>%
  mutate_at('name', list(~ factor(str_remove(., 'Dim'),
                                  levels = as.character(1:plot_dims),
                                  labels = str_c(as.character(1:plot_dims), 
                                                 ' (', round(unname(mca_list[[5]]$eig[1:max(dims_range),'cumulative percentage of variance']),1), '%)')))) %>%
  ggplot(aes(x = value, colour = name)) +
  geom_freqpoly(binwidth = 0.1) +
  scale_color_discrete(name = 'MCA dimension (*)') +
  labs(x = 'score',
       caption = '*Cumulative proportion of variance explained by MCA dimensions given in brackets.') +
  theme_elsie()
ggsave(filename = glue::glue("mca_scores_{plot_dims}.png"),
       path = main_results_path,
       width = plot_width, height = 12, units = 'cm')
  