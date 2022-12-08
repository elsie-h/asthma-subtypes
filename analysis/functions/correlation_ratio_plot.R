# interpret components

source(here::here("analysis", "functions", "sanitise_features.R"))

correlation_ratio_plot <- function(mca_res, x = 5, dims = 14, title, dim_final=7) {
  
  as_tibble(mca_res$var$eta2, rownames = 'var') %>% 
    select(var, everything()) %>%
    pivot_longer(cols = 2:ncol(.)) %>%
    mutate_at('var', list(~ as.factor(str_remove(., 'term_')))) %>%
    mutate(dim = as.numeric(str_remove(name, 'Dim '))) %>%
    filter(dim <= dims) %>%
    group_by(var) %>%
    mutate(total = sum(value)) %>%
    ungroup() %>%
    sanitise_features(var_names = 'var') %>%
    mutate(
      dim_grouped = factor(
        if_else(dim <= dim_final,
                FALSE, 
                TRUE),
        levels = c(FALSE, TRUE),
        labels = c(glue::glue("1 - {dim_final}"), glue::glue("{dim_final+1} - {dims}"))
        )
      ) %>%
    group_by(var, dim_grouped) %>%
    summarise(value_grouped = sum(value), .groups="keep") %>%
    ungroup(dim_grouped) %>%
    mutate(total = sum(value_grouped)) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(x = reorder(var, total), 
                 y = value_grouped,
                 fill = dim_grouped),
             stat = 'identity',
             position = position_stack(reverse = TRUE)) +
    scale_fill_discrete(name = "MCA dimensions") +
    labs(y = 'correlation ratio',
         x = '',
         title = title) +
    coord_flip() +
    theme_elsie() +
    theme(legend.position = 'bottom')
  
}
