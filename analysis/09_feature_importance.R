################################################################################
# Feature importance

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load model
rf_res_final <- readr::read_rds(file.path(main_results_path, "rf_final.RDS"))

# load subtype labels
subtype_labels <- readr::read_rds(file.path(main_results_path, "subtype_labels.RDS"))

# load functions
source(here::here("analysis", "functions", "sanitise_features.R"))

################################################################################
plot_data <- as_tibble(
  randomForest::importance(rf_res_final), 
  rownames = 'feature'
  ) %>%
  select(-MeanDecreaseAccuracy, -MeanDecreaseGini) %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  group_by(name) %>%
  top_n(10, value) %>%
  ungroup() %>%
  arrange(name, value) %>%
  mutate(order = row_number()) %>%
  mutate_at('name', factor) %>%
  sanitise_features('feature') %>%
  mutate_at('feature', list(~ str_remove(., ' cat')))

plot_data %>%
  ggplot(aes(x = order, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ name, scales = "free_y", ncol = 2) +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  scale_fill_discrete(guide=FALSE) +
  # Add categories to axis
  scale_x_continuous(
    breaks = plot_data$order,
    labels = plot_data$feature,
    expand = c(0,0)) +
  coord_flip()
ggsave(filename = 'rf_importance.png',
       path = main_results_path,
       width = plot_width, height = 18, units = 'cm')

