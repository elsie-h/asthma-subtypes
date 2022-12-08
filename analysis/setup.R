# setup

# libraries
library(tidyverse)
# library(lubridate)
# library(cowplot)
# library(RColorBrewer)
# library(fastmap) # otherwise FactoMineR fails
# library(FactoMineR)
# library(fpc)
# library(scales)
# library(mltest)
# library(cluster)
# library(randomForest)
# library(Rtsne)
# library(glue)
# library(cluster)

# create output directory
outdir <- here::here("output")
main_results_path <- here::here("output", "main")
validation_results_path <- here::here("output", "validation")
fs::dir_create(main_results_path)
fs::dir_create(validation_results_path)

select_features <- readr::read_rds(here::here("analysis", "lib", "select_features.RDS"))

parameters <- list()
parameters[["dataset_size"]] <- 5000 # in the real data this was 50,000, but reduce to 5,000 here to speed up run times
parameters[["subset_size"]] <- 1000 # in the real data this was 10,000, but reduce to 1,000 here to speed up run times
parameters[["k_range"]] <- 2:12
parameters[["J_threshold"]] <- 0.85 # threshold for Jaccard coefficient for clustering to be considered "stable"

seed <- 2345

caption_width <- 95
plot_width <- 14

# # supplementary features
# supp_features <- c('index_age', 'ethnicity', 'imd_decile', 'RCP3Q',
#                    'sleep', 'activities', 'day', 'cci')

# keep_objects <- c("keep_objects", ls())
