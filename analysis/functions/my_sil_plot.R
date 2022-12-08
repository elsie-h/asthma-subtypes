my_sil_plot <- function (sil.obj) {
  
  if (inherits(sil.obj, c("eclust", "hcut", "pam", "clara", "fanny"))) {
    df <- as.data.frame(sil.obj$silinfo$widths, stringsAsFactors = TRUE)
  }
  else if (inherits(sil.obj, "silhouette")) {
    df <- as_tibble(sil.obj[, 1:3], stringsAsFactors = TRUE)
  } else {
    stop("Don't support an oject of class ", class(sil.obj))
  }
  
  df <- df[order(df$cluster, -df$sil_width), ]
  if (!is.null(rownames(df))) {
    df$name <- factor(rownames(df), levels = rownames(df))
  } else {
    df$name <- as.factor(1:nrow(df))
  }
  
  df$cluster <- as.factor(df$cluster)
  
  p <- df %>%
    mutate(index = as.numeric(name)) %>%
    select(-neighbor, -name) %>%
    mutate(global = mean(sil_width)) %>%
    group_by(cluster) %>% 
    mutate(`per-cluster` = mean(sil_width)) %>%
    ungroup() %>%
    pivot_longer(cols = c('global', 'per-cluster')) %>%
    mutate_at('sil_width', list(~ if_else(name %in% 'global', NA_real_, .))) %>%
    mutate_at('cluster', list(~ if_else(name %in% 'global', '0', as.character(.)))) %>%
    filter(!(cluster %in% '0')| 
           ((cluster %in% '0') & (index==1 | index==nrow(df)))) %>%
    mutate_at('cluster', as.factor) %>%
    mutate_at('name', list(~ factor(.,
                                    levels = c('per-cluster', 'global')))) %>%
    ggplot() +
    geom_bar(aes(x=index, y = sil_width, colour = cluster, fill = cluster),
             stat='identity') +
    geom_line(aes(x=index, y = value, group = cluster, linetype = name)) +
    labs(y = "silhouette width", 
         x = "", 
         subtitle = str_c(n_distinct(df$cluster), ' clusters'),
         caption = str_c('Global average silhouette width = ', round(mean(df$sil_width),2))) + 
    scale_y_continuous(breaks = seq(-1,1,0.5), limits = c(NA,1)) +
    # ggplot2::ylim(c(NA, 1)) + 
    scale_colour_discrete(guide=FALSE) +
    scale_fill_discrete(guide=FALSE) +
    scale_linetype_discrete(name = 'average silhouette width') +
    theme(legend.position = 'bottom', 
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.caption = element_text(size = 8),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
  return(p)
  
}
