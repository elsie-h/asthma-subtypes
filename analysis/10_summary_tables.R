################################################################################
# summarise test2016 across subtypes

################################################################################
# load setup
source(here::here("analysis", "setup.R"))

# load data
test2016 <- readr::read_rds(file.path(main_results_path, "test2016.RDS"))
pred2016 <- readr::read_rds(file.path(main_results_path, "pred2016.RDS"))

final_parameters <- readr::read_rds(here::here(main_results_path, "final_parameters.RDS"))
final_k <- final_parameters[["k"]]

# load subtype labels
subtype_labels <- readr::read_rds(file.path(main_results_path, "subtype_labels.RDS"))

################################################################################
# summarise all features across subtypes
data_in <- test2016 %>%
  mutate(
    subtype = factor(
      pred2016,
      levels = as.character(1:final_k),
      labels = subtype_labels
      )
    )

cluster_summary_table <- tableone::CreateTableOne(
  vars = select_features,
  strata = "subtype",
  data = data_in,
  test = FALSE
)
readr::write_rds(cluster_summary_table, file.path(main_results_path, "cluster_summary_table.RDS"))  
