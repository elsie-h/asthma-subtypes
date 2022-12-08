################################################################################
# the following scripts can be run in order to replicate the analysis reported in
# https://doi.org/10.1016/j.ijmedinf.2022.104942 

# IMPORTANT: restart your R session in between running each script!

################################################################################
# setup: 
# restore all packages in the state required to re-run the scripts in this repo

# install renv if not already installed
# install.packages("renv")

# renv::init()

# restore all packages in state required
renv::restore()

################################################################################
### data preparation ###
# create dummy data
source(here::here("analysis", "00_dummy_data.R"))

# preprocessing
source(here::here("analysis", "01_preprocessing.R"))

################################################################################
### dimensionality reduction ###
# MCA
source(here::here("analysis", "02_MCA.R"))

################################################################################
### cluster analysis ###
# k-means stability
source(here::here("analysis", "03_kmeans_stability.R"))

# silhouettes
source(here::here("analysis",  "04_silhouettes.R"))

# gap statistics
source(here::here("analysis", "05_gap_stats.R"))

################################################################################
### classification ####
# classification tuning
source(here::here("analysis", "06_classification_tuning.R"))

# classification for model selection
source(here::here("analysis", "07_classification_selection.R"))

# classification for final model
source(here::here("analysis", "08_classification_final.R"))

################################################################################
### subtype interpretation ###
# feature importance
source(here::here("analysis", "09_feature_importance.R"))

# summarise subtypes
source(here::here("analysis", "10_summary_tables.R"))

# tsne
source(here::here("analysis", "11_tsne.R"))

################################################################################
### validation ###
# internal validation
source(here::here("analysis", "12_internal_validation.R"))

# external validation   
source(here::here("analysis", "13_external_validation.R"))

# summarise the variables across the 3 years of the internal dataset 
# and across the internal and external datasets
source(here::here("analysis", "14_summarise_3years.R"))
