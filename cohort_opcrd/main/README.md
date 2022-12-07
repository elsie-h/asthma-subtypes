# deriving the datastes from OPCRD

- The scripts [11_eh_d_index.R]() to [33_eh_d_cci.R]() must be run sequentially to create the tables in OPCRD, which can be done via [10_run_eh_d_scripts]().
- The script [50_join_all.R]() reads all the tables into R, joins them to form a dataset and cleans the dataset.
