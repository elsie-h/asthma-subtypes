################################################################################
#  t-SNE

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load data
train2016 <- readr::read_rds(file.path(main_results_path, "train2016.RDS"))
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))

scores_train2016 <- readr::read_rds(file.path(main_results_path, "scores_train2016.RDS"))
scores_test2016 <- readr::read_rds(file.path(main_results_path, "scores_test2016.RDS"))

pred2016 <- readr::read_rds(file.path(main_results_path, "pred2016.RDS"))

# load functions
source(here::here("analysis", "functions", "theme_elsie.R"))

# load subtype labels
subtype_labels <- readr::read_rds(file.path(main_results_path, "subtype_labels.RDS"))

# load parameters
dims_range <- readr::read_rds(file.path(main_results_path, "dims_range.RDS"))
rf_parameters <- readr::read_rds(file.path(main_results_path, "rf_parameters.RDS"))
final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_dims <- final_parameters[["dim"]]
final_k <- final_parameters[["k"]]

################################################################################
# define tsne function
my_tsne <- function(
    data, 
    perp = 30, th = 0.5, it = 1000, seed = 2345, full = FALSE, # TRUE if using the full sample (very slow)
    filename = NULL
    ) { 
  
  if (is.null(filename)) filename <- str_c('tsne_res', perp, str_remove(th, '\\.'), seed, sep = '_')
  if (full) filename <- str_c('full_', filename)
  
  set.seed(seed)
  
  res <- Rtsne::Rtsne(
    X = data, 
    dims = 2, 
    perplexity = perp, 
    theta = th, # speed/accuracy trade-off, 0 for exact t-sne
    check_duplicates = FALSE,
    pca = FALSE, 
    max_iter = it,
    verbose = TRUE, 
    is_distance = FALSE,
    Y_init = NULL, # use random initiation 
    normalize = TRUE, 
    momentum = 0.5, 
    final_momentum = 0.8, 
    eta = 200,
    exaggeration_factor = 12, 
    num_threads = 0 # use all available cores
  ) 
  readr::write_rds(res, file.path(main_results_path, glue::glue("{filename}.RDS")))
  
}

##############################################################################
# tune t-SNE hyperparameters
# I ran t-SNE with Barnes-Hut approximation for theta = 0.5,0.3,0.1 and perplexity=30,40,50.
# ([Kobak2019](https://doi.org/10.1038/s41467-019-13056-x) suggests n/100 as 'large' perplexity)
# Note that I only ran this for subsample 1.

seeds <- c(45,5645,678,190)
perplexities <- c(30,70,100) # max perplexity = 1% of sample size
thetas <- c(0.5, 0.3, 0.1)
tsne_res <- lapply(
  seeds,
  function(s)
    lapply(
      perplexities,
      function(p)
        lapply(
          thetas,
          function(t)
            my_tsne(
              data = scores_train2016[sample(x = nrow(scores_train2016), size = parameters[["subset_size"]]),1:final_dims], 
              perp = p, 
              th = t,
              seed = s,
              it = 2000
              )
          )
      )
  )

res_cost <- tibble(
  cost = numeric(),
  iteration = integer(),
  perplexity = integer(),
  theta = numeric(),
  seed = integer()
  )
for (s in seeds) {
  for (t in thetas) {
    for (p in perplexities) {
      
      filename <- str_c('tsne_res', p, str_remove(t, '\\.'), s, sep = '_')
      tsne_res <- readr::read_rds(file.path(main_results_path, glue::glue("{filename}.RDS")))
      res_cost <- res_cost %>%
        bind_rows(
          tibble(
            cost = tsne_res$itercosts,
            iteration = seq(50,2000,50),
            perplexity = p,
            theta = t,
            seed = s)
          )
    }
  }
}


res_cost %>%
  filter(iteration > 400) %>%
  pivot_longer(cols = seed) %>%
  mutate_at(vars(c('perplexity', 'theta')), as.factor) %>%
  mutate_at('value', list(~ factor(., labels = c('A', 'B', 'C', 'D'))))  %>%
  ggplot(aes(x = iteration, y = cost, colour = perplexity, linetype = theta)) +
  geom_line() +
  facet_wrap(~ value) +
  labs(caption = str_wrap('Hyperparameter tuning was carried out on three subsamples of 10,000 patients (corresponding to subplots A-D) to reduce computation time.', width = caption_width)) +
  theme_elsie()
ggsave(filename = "tsne_tuning.png",
       path = main_results_path,
       height = 10,
       width = plot_width,
       units = 'cm')

##############################################################################
# plot for 4 seeds and 4 samples
seeds <- c(451,156,61,90)
perplexities <- 100 # max perplexity = 1% of sample size
thetas <- 0.1
tsne_res <- lapply(
  seeds,
  function(s)
    lapply(
      perplexities,
      function(p)
        lapply(
          thetas,
          function(t)
            my_tsne(
              data = scores_test2016[sample(x = nrow(scores_test2016), size = parameters[["subset_size"]]),1:final_dims], 
              perp = p, 
              th = t,
              seed = s,
              it = 2000
              )
          )
      )
  )

##############################################################################
# plot results across 4 subsets
tsne_res <- tibble()
for (s in seeds) {
  set.seed(s)
  tmp <- readr::read_rds(file.path(main_results_path, glue::glue("tsne_res_100_01_{s}.RDS")))$Y
  tsne_res <- tsne_res %>%
    bind_rows(
      tibble(V1 = tmp[,1], V2 = tmp[,2]) %>%
        mutate(seed = s) %>%
        mutate(
          subtype = factor(
            pred2016[sample(x = nrow(scores_test2016), size = parameters[["subset_size"]])],
            levels = as.character(1:final_k)
          )
        )
      )
}

tsne_res %>%
  mutate(xvar=V1, yvar=V2) %>%
  mutate_at('seed', list(~ factor(., labels = c('A', 'B', 'C', 'D'))))  %>%
  ggplot(aes(x = xvar, y = yvar, colour = subtype)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ seed, nrow = 2) +
  scale_colour_discrete(guide = FALSE) +
  labs(x = "t-SNE variable 1", y = "t-SNE variable 2") +
  theme_elsie()
ggsave(filename = "tsne_4.png",
       path = main_results_path,
       height = 14,
       width = plot_width,
       units = 'cm')

##############################################################################
tsne_theme <- function(...) {
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 8),
        plot.background = element_blank(),
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        ...)
}

tsne_feature_plots <- function(.data, var_name, legend_name) {
  
  title_width <- 35
  if (nchar(legend_name) > title_width) {
    legend_name <- str_wrap(legend_name,title_width)
  } else {
    legend_name <- str_c(legend_name,"\n")
  }
  
  .data %>%
    ggplot() +
    geom_point(
      aes(x = V1, y = V2, colour = !! sym(var_name)), 
      size = 0.3, alpha=0.5
      ) +
    scale_colour_viridis_d(
      name = legend_name,
      direction = -1
    ) +
    guides(
      colour = guide_legend(
        title.position = 'top',
        nrow=1,
        byrow = TRUE,
        override.aes = list(size=2, alpha=1)
        )
      ) +
    coord_fixed() +
    labs(x = "t-SNE variable 1", y = "t-SNE variable 2") +
    lims(x = c(-80,85), y = c(-60,50)) +
    tsne_theme()
  
}

p_subtypes <- tsne_res %>%
  filter(seed == s) %>%
  mutate_at('subtype', factor, labels = str_wrap(subtype_labels, 40)) %>%
  ggplot() +
  geom_point(aes(x = V1, y = V2, colour = subtype), 
             size = 0.3, alpha=0.5) +
  guides(colour = guide_legend(title.position = 'top',
                               ncol = 1,
                               override.aes = list(size=2, alpha=1))) +
  coord_fixed() +
  labs(x = "t-SNE variable 1", y = "t-SNE variable 2") +
  lims(x = c(-80,85), y = c(-60,50)) +
  tsne_theme(
    legend.key.size = unit(0.7,"cm")
  ) 

tsne_legend <- cowplot::get_legend(p_subtypes)
p_subtypes <- p_subtypes + theme(legend.position = "none")



# feature plots
set.seed(s)
plot_sample <- sample(x = nrow(scores_test2016), size = parameters[["subset_size"]])
p_ar <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "ar_cat",
    legend_name = 'number of asthma reviews in two-year pre-index period'
    )


p_copd <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "COPD",
    legend_name = 'COPD diagnosis'
    )

p_ics <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "ICS_cat",
    legend_name = 'number of ICS prescriptions in pre-index year'
    )

# SABA
p_saba <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "SABA_cat",
    legend_name = 'number of SABA prescriptions in pre-index year'
    )

# OCS
p_ocs <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "OCS_cat",
    legend_name = 'number of OCS prescriptions for asthma in pre-index year'
    )

# RCP3Q
p_rcp3q <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  mutate_at("RCP3Q", factor, levels = c("missing", "0", "1", "2", "3")) %>%
  tsne_feature_plots(
    var_name = "RCP3Q",
    legend_name = 'RCP3Q score'
    )

# exacerbation
p_exacerbation <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  tsne_feature_plots(
    var_name = "exacerbation_cat",
    legend_name = 'number of exacerbations in two-year pre-idex period'
    )

# BMI
p_bmi <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  mutate_at("BMI", 
            list(~ factor(
              case_when(. %in% c("<20", "20-24", "25-29") ~ "<30",
                        . %in% c("30-39", "40+") ~ ">=30",
                        TRUE ~ "missing"),
              levels = c("missing", "<30", ">=30")
            ))) %>%
  tsne_feature_plots(
    var_name = "BMI",
    legend_name = 'BMI (kg/m^2)'
    )

# cci
p_cci <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(
    test2016[plot_sample,] %>%
      mutate_at('cci', 
                list(~ case_when(. < 2 ~ "0-1",
                                 TRUE ~ "2+"))
                )
    ) %>%
  tsne_feature_plots(
    var_name = "cci",
    legend_name = 'CCI'
    )

# allergy
p_allergy <- tsne_res %>%
  filter(seed == s) %>%
  bind_cols(test2016[plot_sample,]) %>%
  mutate(allergy = case_when(
    (food_allergy %in% "absent") & (anaphylaxis %in% "absent") ~ "neither",
    (food_allergy %in% "absent") | (anaphylaxis %in% "absent") ~ "either",
    TRUE ~ "both"
  )
  ) %>%
  mutate(across(allergy, factor, levels = c("neither", "either", "both"))) %>%
  tsne_feature_plots(
    var_name = "allergy",
    legend_name = 'food allergy and/or anaphylaxis')


cowplot::plot_grid(
  p_subtypes, tsne_legend, p_ics, p_ar, p_copd, p_cci,
  labels = c('A', '', 'B', 'C', 'D', 'E'),
  nrow = 3
  )
ggsave(filename = 'tsne_plots_main.pdf',
       path = main_results_path,
       height = 18,
       width = plot_width,
       units = 'cm')
ggsave(filename = 'tsne_plots_main.png',
       path = main_results_path,
       height = 18,
       width = plot_width,
       units = 'cm')

cowplot::plot_grid(
  p_saba, p_ocs, p_rcp3q, p_exacerbation, p_allergy, p_bmi,  
  labels = c('A', 'B', 'C', 'D', 'E', 'F'),
  nrow = 3
  )
ggsave(filename = 'tsne_plots_supplement.png',
       path = main_results_path,
       height = 18,
       width = plot_width,
       units = 'cm')
