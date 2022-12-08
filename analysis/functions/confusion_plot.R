source(here::here("analysis", "functions", "theme_elsie.R"))

confusion_plot <- function(
    .data, xvar, yvar,
    xlab = NULL, ylab = NULL,
    title = NULL, caption = NULL,
    accuracy = FALSE
    ) {
  
  if (accuracy) {
    cm <- .data %>%
      select(actual = {{xvar}}, predicted = {{yvar}})
    # create the confusion matrix
    cm <- as.matrix(table(cluster = cm$actual, predicted = cm$predicted))
    # number of classes
    nc <- nrow(cm) 
    # number of correctly classified instances per class 
    diag <- diag(cm) 
    # number of instances per cluster
    rowsums <- apply(cm, 1, sum) 
    balanced_accuracy <- round(100*(1/nc)*sum(diag/rowsums),1)
    subtitle <- str_c(' (balanced accuracy = ', balanced_accuracy, '%)')
  }
  
  tmp <- .data %>%
    group_by({{yvar}},{{xvar}}) %>%
    count() %>%
    ungroup({{xvar}}) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ungroup()
  
  tmp %>% 
    expand({{xvar}}, {{yvar}}) %>%
    left_join(tmp) %>%
    mutate_at(vars(c('n', 'percent')),
              list(~ if_else(is.na(.), 0, as.numeric(.)))) %>%
    mutate_at(1:2, as.factor) %>% 
    mutate_at('n', list(~ case_when(n %in% 0 ~ '0',
                                    n<5 ~ '<5', 
                                    TRUE ~ scales::comma(., accuracy = 1)))) %>%
    ggplot(aes(x = {{xvar}},
               y = reorder({{yvar}}, -as.numeric({{yvar}})),
               fill = percent)) +
    geom_tile() +
    geom_text(aes(label = n), colour = 'black') +
    scale_fill_distiller(name = 'row-wise\npercent',
                         palette = 'Spectral',
                         limits = c(0,100)) +
    labs(x = xlab,
         y = ylab,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_elsie() 
  
}

validation_confusion_plots <- function(
    validation = "internal",
    data_2016,
    data_2017,
    data_2018,
    order_2016 = 1:final_k,
    order_2017 = 1:final_k,
    order_2018 = 1:final_k
) {
  
  if (validation == "internal") {
    titleA <- "2016 test dataset"
    titleB <- "2017 internal validation dataset"
    titleC <- "2018 internal validation dataset"
  } else if (validation == "external") {
    titleA <- "2016 external validation dataset"
    titleB <- "2017 external validation dataset"
    titleC <- "2018 external validation dataset"
  }
  
  ylab_label <- "assigned subtype\n(random forest)"
  
  relabel_clusters <- function(.data, order) {
    
    .data %>%
      # use the factor function to swap the labels
      mutate_at(
        "cluster", 
        list(
          ~factor(., levels = 1:final_k, labels = as.character(order))
        )
      ) %>%
      # convert to numeric using the new labels
      mutate_at(
        "cluster", 
        list(
          ~as.numeric(as.character(.))
        )
      ) %>%
      # convert both back to factors (levels should now correspond)
      mutate_at(vars(c("cluster", "predictions")), as.factor)
    
  }
  
  cp_2016 <-  data_2016 %>%
    relabel_clusters(order = order_2016) %>%
    confusion_plot(
      yvar = predictions, xvar = cluster, 
      ylab = ylab_label, xlab = "", title = titleA,
      accuracy = TRUE
    ) +
    theme(legend.position = "none")
  
  cp_2017 <- data_2017 %>%
    relabel_clusters(order = order_2017) %>%
    confusion_plot(
      yvar = predictions, xvar = cluster,
      ylab = ylab_label, xlab = "", title = titleB,
      accuracy = TRUE
    ) +
    theme(legend.position = "bottom")
  
  cp_legend <- cowplot::get_legend(cp_2017)
  cp_2017 <- cp_2017 + theme(legend.position = "none")
  
  cp_2018 <- data_2018 %>%
    relabel_clusters(order = order_2018) %>%
    confusion_plot(
      yvar = predictions, xvar = cluster,
      ylab = ylab_label, xlab = "cluster label (k-means)",
      title = titleC,
      accuracy = TRUE
    ) +
    theme(legend.position = "none")
  
  cowplot::plot_grid(
    cowplot::plot_grid(
      cp_2016, cp_2017, cp_2018, labels = c("A", "B", "C"), nrow=3
    ),
    cp_legend, nrow = 2, rel_heights = c(8,1)
  )
  
}

