################################################################################
# plot key features across 3 years for the internal dataset 
# and across the internal and external 2016 datasets

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load output from internal datasets
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))
test2017 <- readr::read_rds(file.path(main_results_path, "test2017.RDS"))
test2018 <- readr::read_rds(file.path(main_results_path, "test2018.RDS"))

test_pred2016 <- readr::read_rds(file.path(main_results_path, "pred2016.RDS"))
test_pred2017 <- readr::read_rds(file.path(main_results_path, "pred2017.RDS"))
test_pred2018 <- readr::read_rds(file.path(main_results_path, "pred2018.RDS"))

# load output from external datasets
validation2016 <- readr::read_rds(file.path(validation_results_path, "validation2016.RDS"))
validation2017 <- readr::read_rds(file.path(validation_results_path, "validation2017.RDS"))
validation2018 <- readr::read_rds(file.path(validation_results_path, "validation2018.RDS"))

validation_pred2016 <- readr::read_rds(file.path(validation_results_path, "pred2016.RDS"))
validation_pred2017 <- readr::read_rds(file.path(validation_results_path, "pred2017.RDS"))
validation_pred2018 <- readr::read_rds(file.path(validation_results_path, "pred2018.RDS"))

# load model
rf_res_final <- readr::read_rds(file.path(main_results_path, "rf_final.RDS"))

# load parameters
final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_k <- final_parameters[["k"]]

# load subtype labels
subtype_labels <- readr::read_rds(file.path(main_results_path, "subtype_labels.RDS"))

# load functions
source(here::here("analysis", "functions", "sanitise_features.R"))
source(here::here("analysis", "functions", "sanitise_categories.R"))
source(here::here("analysis", "functions", "theme_elsie.R"))

################################################################################
# percent of each dataset in each subtype
subtype_percents <- tibble(
  subtype = 1:final_k,
  internal_2016 = as.vector(table(test_pred2016)),
  internal_2017 = as.vector(table(test_pred2017)),
  internal_2018 = as.vector(table(test_pred2018)),
  external_2016 = as.vector(table(validation_pred2016)),
  external_2017 = as.vector(table(validation_pred2017)),
  external_2018 = as.vector(table(validation_pred2018))
  ) %>%
  mutate_at(
    vars(starts_with(c("internal", "external"))), 
    list(~ str_c(round(100*./sum(.),1), "%"))
    ) %>%
  pivot_longer(
    cols = starts_with(c("internal", "external")),
    names_pattern = "(.*)_(.*)",
    names_to = c("dataset", "year")
    ) %>%
  mutate_at(vars(c("year", "subtype")), as.integer) %>%
  rename(subtype_percent = value)

################################################################################
# split across feature categories within subtypes
summarise_var <- function(v) {
  
  tmp_fun <- function(data, preds, var) {
    
    tmp1 <- data %>%
      mutate(subtype = as.integer(preds)) %>%
      group_by_at(vars(c("subtype", var))) %>%
      count() %>%
      ungroup() %>%
      group_by(subtype) %>%
      mutate(percent = 100*n/sum(n)) %>%
      ungroup() %>%
      select(-n) 
    
    tmp2 <- tibble()
    for (s in 1:final_k) {
      tmp2 <- bind_rows(data[preds!=s,] %>%
                           group_by_at(var) %>%
                           count() %>% 
                           ungroup() %>%
                           mutate(percent_of_total =  100*n/sum(n)) %>%
                           mutate(subtype=s) %>%
                           select(-n),
                         tmp2)
    }
    
    tmp1 <- tmp1 %>%
      left_join(tmp2, by = c("subtype",var)) %>%
      pivot_longer(cols = var)
    
  }
  
  bind_rows(
    bind_rows(
      tmp_fun(test2016, test_pred2016, v) %>% mutate(year=2016L),
      tmp_fun(test2017, test_pred2017, v) %>% mutate(year=2017L),
      tmp_fun(test2018, test_pred2018, v) %>% mutate(year=2018L) 
    ) %>%
      mutate(dataset = "internal"),
    bind_rows(
      tmp_fun(validation2016, validation_pred2016, v) %>% mutate(year=2016L),
      tmp_fun(validation2017, validation_pred2017, v) %>% mutate(year=2017L),
      tmp_fun(validation2018, validation_pred2018, v) %>% mutate(year=2018L)
      ) %>% 
      mutate(dataset = "external")
    )
  
}

subtype_summaries <- bind_rows(
  lapply(select_features, summarise_var)
  ) %>%
  left_join(
    subtype_percents, by = c("subtype", "dataset", "year")
  ) %>%
  mutate_at("dataset", factor, levels = c("internal", "external"))

plot_summary <- function(.data, by = "year", s) {
  
  # select top 10 important features for subtype
  important_features <- as_tibble(
    randomForest::importance(rf_res_final), 
    rownames = "feature"
    ) %>%
    arrange_at(as.character(s), desc) %>%
    slice(1:10) 
  
  # define title
  title <- str_c("Subtype ", str_replace(subtype_labels[s], "\\.", "\\:"))
  
  # prepare data
  tmp <- .data %>%
    filter(subtype == s) %>%
    filter(name %in% important_features$feature) %>%
    mutate_at("value", as.character) %>%
    sanitise_features("name") %>%
    sanitise_categories("value") %>%
    mutate_at("value", list(~ str_remove(., " cells per microlitre"))) %>%
    mutate(facet_var = factor(str_c(name, ": ", value))) %>%
    mutate_at("year", factor) 
  
  # plot
  if (by == "year") {
    
    # percent for legend label
    legend_label <- tmp %>%
      distinct(year, subtype_percent) %>%
      transmute(str_c(year, " (", subtype_percent, ")")) %>%
      unlist() %>% unname()
    
    # plot
    p <- tmp %>%
      ggplot(aes(x = facet_var, fill = reorder(year, -as.numeric(year)))) +
      geom_bar(aes(y = percent_of_total/100), alpha = 0.3,
               stat="identity", 
               width = 0.5, position = position_dodge(0.5)) +
      geom_bar(aes(y = percent/100), 
               stat="identity",  
               width = 0.2, position = position_dodge(0.5)) +
      coord_flip() +
      scale_fill_discrete(breaks=c("2016","2017","2018"), 
                          labels = legend_label,
                          name="Year") 
    
  } else if (by == "dataset") {
    
    # percents for legend label
    legend_label <- tmp %>%
      distinct(dataset, subtype_percent) %>%
      transmute(str_c(dataset, " (", subtype_percent, ")")) %>%
      unlist() %>% unname()
    
    # plot
    p <- tmp %>%
      mutate_at("dataset", factor) %>%
      ggplot(aes(x = facet_var, fill = reorder(dataset, -as.numeric(dataset)))) +
      geom_bar(aes(y = percent_of_total/100), alpha = 0.3,
               stat="identity",
               width = 0.5, position = position_dodge(0.5)) +
      geom_bar(aes(y = percent/100), 
               stat="identity",  
               width = 0.2, position = position_dodge(0.5)) +
      coord_flip() +
      scale_fill_discrete(
        name="Data",
        breaks = c("internal", "external"),
        labels = legend_label
        )
    
  }
  
  p <- p +
    scale_x_discrete(limits = rev(levels(tmp$facet_var)), name=NULL) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0,0),
                       limits = c(0,1), name=NULL) +
    labs(subtitle = title) +
    theme_elsie() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
      )
  
  ggsave(p,
         filename = glue::glue("main_summary_{by}_{s}.png"),
         path = validation_results_path,
         height = 17,
         width = plot_width,
         units = "cm")
  
}

for (i in 1:final_k) {
  
  subtype_summaries %>%
    filter(dataset =="internal") %>%
    plot_summary(s = i, by = "year")
  
  subtype_summaries %>%
    filter(year == 2016) %>%
    plot_summary(s = i, by = "dataset")

  }
