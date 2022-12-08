################################################################################
# plot silhouette widths for the stable solutions
################################################################################

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# define parameters
k_range <- parameters[["k_range"]] 
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))

# load functions
source(here::here("analysis", "functions", "my_sil_plot.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

# load data
scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))

################################################################################

# randomly select half of the samples
set.seed(seed)
subsample <- sample(x = 1:nrow(scores_train2016), size = nrow(scores_train2016)/2)

################################################################################
# empty list for all stable results
k_stable_list <- list()
for (dim in dims_range) {
  
  # read cluster results
  clusterboot_res <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{dim}.RDS")))
  # empty list for silhouette plots
  sil_list <- list()
  i <- 1
  # calculate squared Euclidean distances
  d <- dist(scores_train2016[subsample,1:dim])^2
  
  #### plot silhouette widths 
  # only allow 8 subplots per plot, if more than 8 stable k, split over two plots
  # empty list for stable results for dims
  k_stable_dims <- integer()
  for (k in k_range) {
    keep <- k-1
    if (all(clusterboot_res[[keep]]$subsetmean > parameters[["J_threshold"]])) {
      
      k_stable_dims <- c(k_stable_dims, k)
      
      s <- cluster::silhouette(x = clusterboot_res[[keep]]$result$result$cluster[subsample], dist = d)
      
      filename <- str_c('silhouette', dim, k, sep = '_')
      readr::write_rds(s, file.path(main_results_path, glue::glue("{filename}.RDS")))
      
      sil_plot <- my_sil_plot(s) + lims(y=c(-0.5,1))
      
      if (i == 1) {
        legend <- cowplot::get_legend(sil_plot)
        sil_plot <- sil_plot + theme(legend.position = 'none')
      } else {
        sil_plot <- sil_plot + theme(legend.position = 'none')
      }
      
      if (i %in% seq(1, 10, 8)) {
        plot_title <- str_c(dim, ' MCA dimensions')
      } else {
        plot_title <- ''
      }
      
      if (i %in% seq(1, 10, 2)) {
        ylab <- 'silhouette width'
      } else {
        ylab <- ''
      }
      
      sil_list[[i]] <- sil_plot + labs(title = plot_title, y = ylab)
      i <- i+1
    }
  }
  k_stable_list[[dim]] <- k_stable_dims
  
  # plot silhouette results
  sil_len <- length(sil_list)
  plot_rows <- min(ceiling(sil_len/2),4)
  lower <- 1; upper <- 8
  while (lower <= sil_len) {
    upper <- min(upper, sil_len)
    plot_labs <- str_c(toupper(letters)[lower:upper], collapse = '\',\'')
    plot_grid_command <- str_c(
      'cowplot::plot_grid(',
      str_c(str_c('sil_list[[', lower:upper,']]'), collapse = ', '),
      ', nrow = ',plot_rows,', ncol = 2, labels = c(\'', plot_labs,'\'))'
      )
    s_plot <- eval(parse(text=plot_grid_command))
    s_plot <- cowplot::plot_grid(s_plot, legend, nrow=2, rel_heights = c(plot_rows*4,1))
    plot_height <- 4*plot_rows + 1.5
    # save plot
    ggsave(filename = glue::glue("silouhette_{dim}_{lower}.png"),
           path = main_results_path,
           width = plot_width, height = plot_height, units = 'cm')
    # update params
    lower <- lower + 8; upper <- upper + 8
  }
}

readr::write_rds(k_stable_list, file.path(main_results_path, "k_stable_list.RDS"))

################################################################################

average_widths <- tibble()
for (dim in dims_range) {
  for (k in k_range) {
    clusterboot_res <- readr::read_rds(file.path(main_results_path, glue::glue("train_clusterboot_res_{dim}.RDS")))
    keep <- k-1
    if (all(clusterboot_res[[keep]]$subsetmean > parameters[["J_threshold"]])) {
      filename <- str_c('silhouette', dim, k, sep = '_')
      s <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
      average_widths <- bind_rows(average_widths,
                                  tibble(dims = dim, 
                                         k = k, 
                                         average = summary(s)$avg.width))
    }
  }
}

average_widths %>%
  mutate_at('dims', factor)  %>%
  mutate_at('average', list(~ round(., 2))) %>%
  ggplot(aes(x = k, y = average, colour = dims)) +
  geom_point() +
  geom_line() +
  labs(y = 'average silhouette width') +
  scale_colour_discrete(name = 'MCA\ndimensions') +
  theme_elsie() 
ggsave(filename = 'average_silhouette_widths.png',
       path = main_results_path,
       width = plot_width, height = 12, units = 'cm')
