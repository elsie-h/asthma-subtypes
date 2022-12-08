################################################################################
# pre-processing script
################################################################################

# load setup
source(here::here("analysis", "setup.R"))

# read main dummy data
main_dummy_data <- readr::read_rds(here::here("analysis", "data", "main_dummy_data.RDS"))
# read extternal validation dummy data
validation_dummy_data <- readr::read_rds(here::here("analysis", "data", "validation_dummy_data.RDS"))

################################################################################
# process main dummy data
## construct feature for splitting 2016 data into train and test
set.seed(seed)
test_samples <- sample(1:(2*parameters[["dataset_size"]]), size=parameters[["dataset_size"]])
train_samples <- rep('train', 2*parameters[["dataset_size"]])
train_samples[test_samples] <- 'test'

## train and test 2016
data_2016 <- main_dummy_data %>%
  filter(index_year == 2016) %>%
  arrange(patid, index_date) %>%
  mutate(dataset = train_samples)

train2016 <- data_2016 %>% filter(index_year == 2016, dataset == 'train')
readr::write_rds(train2016, file.path(main_results_path, "train2016.RDS"), compress = "gz")

test2016 <- data_2016 %>% filter(index_year == 2016, dataset == 'test')
readr::write_rds(test2016, file.path(main_results_path, "test2016.RDS"), compress = "gz")

## test 2017
test2017 <- main_dummy_data %>% filter(index_year == 2017)
readr::write_rds(test2017, file.path(main_results_path, "test2017.RDS"), compress = "gz")

## train and test 2018
test2018 <- main_dummy_data %>% filter(index_year == 2018)
readr::write_rds(test2018, file.path(main_results_path, "test2018.RDS"), compress = "gz")

################################################################################
# external validation datasets
# validation 2016
validation2016 <- validation_dummy_data %>% filter(index_year == 2016)
readr::write_rds(validation2016, file.path(validation_results_path, "validation2016.RDS"), compress = "gz")

# test 2017
validation2017 <- validation_dummy_data %>% filter(index_year == 2017)
readr::write_rds(validation2017, file.path(validation_results_path, "validation2017.RDS"), compress = "gz")

# train and test 2018
validation2018 <- validation_dummy_data %>% filter(index_year == 2018)
readr::write_rds(validation2018, file.path(validation_results_path, "validation2018.RDS"), compress = "gz")

################################################################################
# summarise all datasets across years
tables_data <- bind_rows(
  train2016 %>% mutate(dataset = "2016 train"),
  test2016 %>% mutate(dataset = "2016 test"),
  test2017 %>% mutate(dataset = "2017 test"),
  test2018 %>% mutate(dataset = "2018 test"),
  validation2016 %>% mutate(dataset = "2016 validation"),
  validation2017 %>% mutate(dataset = "2017 validation"),
  validation2018 %>% mutate(dataset = "2018 validation")
) %>%
  mutate(across(dataset, factor, levels = c("2016 train", "2016 test", "2017 test", "2018 test", "2016 validation", "2017 validation", "2018 validation")))
  
table1 <- tableone::CreateTableOne(
  vars = select_features,
  strata = "dataset",
  data = tables_data,
  test = FALSE
)
readr::write_rds(table1, file.path(outdir, "table1.RDS"))  
