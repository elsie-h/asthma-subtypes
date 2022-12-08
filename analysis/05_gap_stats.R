################################################################################
# calculate gap statistics

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load parameters
# define parameters
k_range <- parameters[["k_range"]] 
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))
k_stable_list <- readr::read_rds(file.path(main_results_path, "k_stable_list.RDS"))

# load data
scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))

# load functions
source(here::here("analysis", "functions", "theme_elsie.R"))

################################################################################
seeds <- c(451,156,61,90)

# calculate gap statistics
gap_res <- tibble()
for (dim in dims_range) {
  
  print(str_c('start Gap stats for ', dim, ' dimensions.'))
  
  k_max <- max(k_stable_list[[dim]]) + 1
  
  for (s in seeds) {
    
    set.seed(s)
    gap_res_s <- cluster::clusGap(
      x = scores_train2016[sample(x = nrow(scores_train2016), size = parameters[["subset_size"]]),1:dim],
      FUN = kmeans, 
      nstart = 25,
      iter.max = 25,
      K.max = k_max, B = 100
      )
    
    gap_res <- bind_rows(
      gap_res,
      as_tibble(gap_res_s$Tab, rownames = 'k') %>% mutate(dims = dim, seed = s)
      )
    
    filename <- str_c('gap_res', dim, s, sep = '_')
    readr::write_rds(gap_res_s, file.path(main_results_path, glue::glue("{filename}.RDS")))
    
  }
}

################################################################################
# plot gap statistics
gap_res <- tibble()
for (dim in dims_range) {
  for (s in c(451,156,61,90)) {
    filename <- str_c('gap_res', dim, s, sep = '_')
    gap_res_s <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
    gap_res <- bind_rows(
      gap_res,
      as_tibble(gap_res_s$Tab, rownames = 'k') %>% mutate(dims = dim, seed = s))
  }
}

max_gap_res <- tibble()
seeds <- c(451,156,61,90)
m <- c("firstSEmax", "Tibs2001SEmax", "globalSEmax", "firstmax", "globalmax")
for (dim in dims_range) {
  for (s in seeds) {
    f_res <- gap_res %>% filter(dims==dim, seed==s)
    m_res <- sapply(
      m,
      function(x) 
        cluster::maxSE(
          f = f_res$gap, 
          SE.f = f_res$SE.sim, 
          method = x,
          SE.factor = 1
          )
      )
    max_gap_res <- bind_rows(
      max_gap_res,
      tibble(method = m, k = m_res, dims = dim, seed= s)
      )
  }
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gap_res %>%
  left_join(
    max_gap_res %>% 
      mutate_at('k', as.character) %>%
      filter(method %in% 'Tibs2001SEmax')
    ) %>%
  mutate(lower = gap-SE.sim, upper = gap+SE.sim) %>%
  mutate_at(vars(c('seed','method')), factor) %>%
  mutate_at('k', as.numeric) %>%
  ggplot(aes(x = k, y = gap, group = seed, colour = seed, shape=method)) +
  geom_line() +
  geom_point(size=2.5, alpha=0.5) +
  facet_wrap(~ dims, scales='free_y') +
  scale_shape_discrete(guide=FALSE) +
  scale_x_continuous(breaks = seq(2,k_max,2)) +
  labs(y='Gap statistic') +
  # caption = str_wrap('Subplots represent MCA dimensions. Colours represent randomly selected samples used in the calculation of the Gap statistics. Points represent the estimated number of clusters for the given MCA dimensions and randomly selected sample. If the point is at k=12, there was no k for which the Gap statistic satisfied the Tibshirani criterion.',caption_width)) +
  theme_elsie() +
  theme(legend.position = 'none')
ggsave(filename = "gap_line.png",
       path = main_results_path,
       height = 12,
       width = plot_width,
       units = 'cm')

