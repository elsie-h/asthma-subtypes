# identify asthma subtypes

The script [00_run_analysis.R]() first reads the data from OPCRD by calling [analysis/cohort_opcrd/main/50_join_all.R](), then implements each stage of the analysis framework.

The following framework exists to avoid saving patient-level data locally:
- The objects `tuning` and `update_files` can be set to `TRUE` or `FALSE` to control which chunks of code in the scripts [01 _preprocessing.R]() to [22_external_validation_plots.R]() run. 
  - Set all to `TRUE` for the first run of each script
  - Set all to `FALSE` to run chunks that create the necessary objects to run a script that is downstream in the analysis framework. For example:
    - [01_preprocessing.R]() and [02_MCA.R]() have run previously with `tuning=TRUE` and `update_files=TRUE`. 
    - To run [03_kmeans_stability.R]() in a new session, the user can run [analysis/cohort_opcrd/main/50_join_all.R](), then [01_preprocessing.R]() and [02_MCA.R]() with `tuning=FALSE` and `update_files=FALSE` to create/load only the objects necessary to run [03_kmeans_stability.R](). 

The objects `thesis` and `manuscript` control the plots that are generated in the scripts. Both should be set to `TRUE` to generate the plots that appear in the paper.
