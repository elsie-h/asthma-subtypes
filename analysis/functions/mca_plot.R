# eigenvalue plot
mca_plot <- function(mca_list, dim_lines, title) {
  
  # cumulative % of variance for train2016
  cpv <- as_tibble(mca_list[[5]]$eig) %>% 
    mutate(rows = 5, 
           dim = row_number()) %>%
    bind_rows() 
  
  cpv_text <- cpv %>%
    filter(dim %in% dim_lines) %>%
    select(rows, dim, cpv = `cumulative percentage of variance`) %>%
    group_by(dim) %>%
    summarise(cpv = mean(cpv), .groups = 'keep') %>%
    ungroup() %>%
    transmute(cpv = str_c(round(cpv,1), '%')) %>%
    unlist()
  
  plot_data <- lapply(seq_along(mca_list), 
                      function(x) as_tibble(mca_list[[x]]$eig) %>% 
                        mutate(rows = x, 
                               dim = row_number(),
                               mean_pv = mean(`percentage of variance`),
                               sd_pv = sd(`percentage of variance`),
                               cutoff = mean_pv+sd_pv)) %>%
    bind_rows() %>%
    mutate(colours = factor(case_when(rows %in% 5 ~ 'train',
                                      TRUE ~ str_c('subsample ', rows)),
                            levels = c(str_c('subsample ', 1:4), 'train'))) %>%
    mutate(linetypes = case_when(colours %in% 'train' ~ 'train',
                                 TRUE ~ 'subsample')) 
  
  max_dims <- nrow(mca_list[[1]]$eig)
  
  # define the coefficient for scaling the axes appropriately
  coeff <- 100/max(plot_data$`percentage of variance`)
  # mutate_at('rows', list(~ )) %>%
  plot_data %>%
    ggplot(aes(x=dim, colour = as.factor(rows))) +
    geom_rect(inherit.aes = FALSE, 
              xmin=dim_lines[1], xmax=dim_lines[2], ymin=-Inf, ymax=+Inf,
              fill='grey', alpha=0.1) +
    geom_line(aes(y=`percentage of variance`,
                  colour = colours,
                  linetype = linetypes)) +
    geom_line(aes(y=`cumulative percentage of variance` / coeff,
                  colour = colours,
                  linetype = linetypes)) +
    geom_hline(yintercept = c(cpv$`cumulative percentage of variance`[dim_lines[1]]/coeff, cpv$`cumulative percentage of variance`[dim_lines[2]]/coeff), colour = 'grey') +
    scale_y_continuous(
      # Features of the first axis
      name = "% variance per component\n(decreasing line)",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="cumulative % variance\n(increasing line)")) +
    scale_linetype_discrete(guide = FALSE) +
    scale_x_continuous(breaks = seq(0, 90, 10),
                       minor_breaks = seq(0, 90, 2)) +
    scale_color_manual(name = NULL,
                       values = c(scales::hue_pal()(4), 'black')) +
    guides(colour = guide_legend(override.aes = list(linetype = c(rep(1,4), 2)))) +
    theme_elsie() +
    theme(legend.position = 'bottom') +
    labs(x = 'MCA dimension',
         title = title) 
  
}
