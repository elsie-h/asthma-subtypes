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
