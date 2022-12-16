################################################################################
# dummy data
################################################################################

# load setup
source(here::here("analysis", "setup.R"))

# create output directories
fs::dir_create(here::here("analysis", "lib"))
fs::dir_create(here::here("analysis", "data"))

# specify features for MCA
select_features <- c(
  # demographics
  "sex_coded", "index_age_q", "smoking_status", "age_onset_cat",
  # physiolical
  "BMI", "eos",
  # healthcare events
  "emergency_cat", "exacerbation_cat", "ar_cat", "URTI_cat", "LRTI_cat",  
  "influenza_vaccination",
  # lung function
  "pefr_pp", "ppFEV1", 
  # management
  "plan_n", "pefr_home",
  # control
  "RCP3Q", 
  # allergy
  "anaphylaxis", "angioedema_or_urticaria", "conjunctivitis", "eczema", "rhinitis", 
  "drug_allergy", "food_allergy", "other_allergy",  "oral_antihistamine",
  # comorbidity
  "Anxiety", "Chronic_cardiac_disease",  "COPD", "Depression",
  "Diabetes", "GORD", "Nasal_polyps", 
  # other prescriptions
  "beta_blocker", "NSAID", "paracetamol", "statin", 
  # asthma prescriptions
  "ICS_cat", "SABA_cat", "LABA_cat", 'LAMA_cat', 'SAMA_cat',
  "LTRA_cat", "methylxanthine_cat", "OCS_cat"
  )

readr::write_rds(select_features, here::here("analysis", "lib", "select_features.RDS"))

create_dummy_data <- function(index_date, n) {
  
  create_cat_var <- function(var_levels, prob = NULL, as.factor=TRUE) {
    
    if (is.null(prob)) {
      prob <- unname(var_levels)
    }
    
    out <- sample(x = names(var_levels), size = n(), replace = TRUE, prob = prob)
    
    if (as.factor) {
      out <- factor(out, levels = names(var_levels))
    } 
    
    return(out)
    
  }
  
  subtype_levels <- c(
    "1" = 0.3, # "low medication and low healthcare utilisation"
    "2" = 0.36, # "low-to-medium medication use"
    "3" = 0.12, # "low-to-medium medication use and comorbidities"
    "4" = 0.05, # "varied medication use and comorbid COPD" 
    "5" = 0.1, # "high medication use"
    "6" = 0.07 #  "very high medication use"
  )
  
  sex_levels <- c("F" = 0.5, "M" = 0.5)
  smoking_status_levels <- c("current" = 1/3, "ex" = 1/3, "non" = 1/3)
  BMI_levels <- c("<20" = 0.1, "20-24" = 0.3, "25-29" = 0.3, "30-39" = 0.2, "40+" = 0.05, "missing" = 0.05)
  eos_levels <- c("<=0.4" = 0.6, ">0.4" = 0.2, "missing" = 0.2)
  emergency_cat_levels <- exacerbation_cat_levels <- c("0" = 0.9, "1+" = 0.1)
  
  index_age_quintiles <- c("18-33" = 0.2, "34-45" = 0.2, "46-56" = 0.2, "57-68" = 0.2, "69-100" = 0.2)
  ar_cat_levels <- c("0" = 0.1, "1" = 0.5, "2" = 0.3, "3+" = 0.1)
  RCP3Q_levels <- c("0" = 0.2, "1" = 0.2, "2" = 0.2, "3+" = 0.2, "missing" = 0.2)
  med4_levels <- c("0" = 0.25, "1-4" = 0.25, "5-8" = 0.25, "9+" = 0.25)
  med2_levels <- c("0" = 0.5, "1+" = 0.5)
  OCS_cat_levels <- c("0" = 0.25, "1" = 0.25, "2" = 0.25, "3+" = 0.25)
  comorb2_levels <- c("absent" = 0.8, "ever" = 0.2)
  comorb3_levels <- c("absent" = 0.8, "ever" = 0.1, "active" = 0.1)
  infection_levels <- c("0" = 0.5, "1" = 0.3, "2+" = 0.2)
  bin_levels <- c("no" = 0.5, "yes" = 0.5)
  pefr_levels <- c("<60%" = 0.1, "60-80%" = 0.2, ">80%" = 0.4, "not valid" = 0.1, "missing" = 0.2)
  fev1_levels <- c("<60%" = 0.1, "60-80%" = 0.2, "80-100%" = 0.2, ">100%" = 0.2, "not valid" = 0.1, "missing" = 0.2)
  age_onset_levels <- c("<18" = 0.5, ">=18" = 0.5)
  
  dummy_data <- tibble(
    patid = 1:n,
    index_date = as.Date(rep(index_date, n)),
    index_year = as.integer(lubridate::year(index_date))
  ) %>%
    mutate(
      
      # create some structure in the dummy data to *loosely* represent the subtypes presented in the paper
      subtype = create_cat_var(subtype_levels),
      
      index_age_q = factor(
        case_when(
          subtype %in% c("1", "2") ~ create_cat_var(index_age_quintiles, prob = c(0.3,0.3,0.2,0.1,0.1), as.factor = FALSE),
          subtype %in% c("3", "4") ~ create_cat_var(index_age_quintiles, prob = c(0.1,0.1,0.2,0.3,0.3), as.factor = FALSE),
          subtype %in% c("5", "6") ~ create_cat_var(index_age_quintiles, as.factor = FALSE)
        ),
        levels = names(index_age_quintiles)
      ),
      
      # ar_cat and RCP3Q to define healthcare utilisation
      # few asthma reviews in subgroups 1
      ar_cat = factor(
        case_when(
          subtype %in% "1" ~ create_cat_var(ar_cat_levels, prob = c(0.88, rep(0.12/3,3)), as.factor = FALSE),
          TRUE ~ create_cat_var(ar_cat_levels, as.factor = FALSE)
        ),
        levels = names(ar_cat_levels)
      ),
      # RCP3Q has similar distribution to asthma reviews, but with some noise added
      RCP3Q = factor(
        case_when(
          ar_cat == "0" ~ create_cat_var(RCP3Q_levels, prob = c(0.9, rep(0.1/4, 4)), as.factor = FALSE),
          ar_cat == "1" ~ create_cat_var(RCP3Q_levels, prob = c(0.1/4, 0.9, rep(0.1/4, 3)), as.factor = FALSE),
          ar_cat == "2" ~ create_cat_var(RCP3Q_levels, prob = c(rep(0.1/4, 2), 0.9, rep(0.1/4, 2)), as.factor = FALSE),
          ar_cat == "3+" ~ create_cat_var(RCP3Q_levels, prob = c(rep(0.1/4, 3), 0.9, 0.1/4), as.factor = FALSE)
        ),
        levels = names(RCP3Q_levels)
      ),
      
      # asthma medication use features
      # ICS based on subtype medication use
      ICS_cat = factor(
        case_when(
          subtype == "1" ~ create_cat_var(med4_levels, prob = c(0.9, rep(0.1/3, 3)), as.factor = FALSE),
          subtype %in% c("2", "3") ~ create_cat_var(med4_levels, prob = c(0.1/3, 0.9, rep(0.1/3, 2)), as.factor = FALSE),
          subtype == "4" ~ create_cat_var(med4_levels, as.factor = FALSE),
          subtype == "5" ~ create_cat_var(med4_levels, prob = c(rep(0.1/3, 2), 0.9, 0.1/3), as.factor = FALSE),
          subtype == "6" ~ create_cat_var(med4_levels, prob = c(rep(0.1/3, 3), 0.9), as.factor = FALSE)
        ),
        levels = names(med4_levels)
      ),
      # SABA and LABA similar to ICS but with noise added
      SABA_cat = factor(
        if_else(
          purrr::rbernoulli(n = n(), p = 0.2),
          create_cat_var(med4_levels, as.factor = FALSE),
          as.character(ICS_cat)
        ),
        levels = names(med4_levels)
      ),
      LABA_cat = factor(
        if_else(
          purrr::rbernoulli(n = n(), p = 0.2),
          create_cat_var(med4_levels, as.factor = FALSE),
          as.character(ICS_cat)
        ),
        levels = names(med4_levels)
      ),
      # no SAMA if no ICS, and higher if in COPD subtype
      SAMA_cat = factor(
        case_when(
          ICS_cat == "0" ~ "0",
          subtype == "4" ~ create_cat_var(med2_levels, as.factor = FALSE),
          TRUE ~ create_cat_var(med2_levels, prob = c(0.8, 0.2), as.factor = FALSE) 
        ),
        levels = names(med2_levels)
      ),
      # LAMA, LTRA and methyxanthine same as SAMA but with random noise added
      LAMA_cat =  factor(
        if_else(
          purrr::rbernoulli(n = n(), p = 0.2),
          create_cat_var(med2_levels, as.factor = FALSE),
          as.character(SAMA_cat)
        ),
        levels = names(med2_levels)
      ),
      LTRA_cat = factor(
        if_else(
          purrr::rbernoulli(n = n(), p = 0.2),
          create_cat_var(med2_levels, as.factor = FALSE),
          as.character(SAMA_cat)
        ),
        levels = names(med2_levels)
      ),
      methylxanthine_cat = factor(
        if_else(
          purrr::rbernoulli(n = n(), p = 0.2),
          create_cat_var(med2_levels, as.factor = FALSE),
          as.character(SAMA_cat)
        ),
        levels = names(med2_levels)
      ),
      OCS_cat = factor(
        case_when(
          subtype == "1" ~ create_cat_var(OCS_cat_levels, prob = c(0.9, rep(0.1/3, 3)), as.factor = FALSE),
          subtype %in% c("2", "3") ~ create_cat_var(OCS_cat_levels, prob = c(0.1/3, 0.9, rep(0.1/3, 2)), as.factor = FALSE),
          subtype == "4" ~ create_cat_var(OCS_cat_levels, as.factor = FALSE),
          subtype == "5" ~ create_cat_var(OCS_cat_levels, prob = c(rep(0.1/3, 2), 0.9, 0.1/3), as.factor = FALSE),
          subtype == "6" ~ create_cat_var(OCS_cat_levels, prob = c(rep(0.1/3, 3), 0.9), as.factor = FALSE)
        ),
        levels = names(OCS_cat_levels)
      ),
      
      # comorbidities with structure
      COPD = factor(
        case_when(
          subtype == "4" ~ "ever",
          TRUE ~ create_cat_var(comorb2_levels, as.factor = FALSE)
        ),
        levels = names(comorb2_levels)
      ),
      Chronic_cardiac_disease = factor(
        case_when(
          subtype == "3" ~ "ever",
          TRUE ~ create_cat_var(comorb2_levels, as.factor = FALSE)
        ),
        levels = names(comorb2_levels)
      ),
      BMI = factor(
        case_when(
          subtype == "3" ~ create_cat_var(BMI_levels, prob = c(0.05, 0.05, 0.3, 0.4, 0.15, 0.05),as.factor = FALSE),
          TRUE ~ create_cat_var(BMI_levels, as.factor = FALSE)
        ),
        levels = names(BMI_levels)
      ),
      
      # all remaining features random
      sex_coded = create_cat_var(sex_levels),
      smoking_status = create_cat_var(smoking_status_levels),
      
      age_onset_cat = create_cat_var(age_onset_levels),
      eos = create_cat_var(eos_levels),
      emergency_cat = create_cat_var(emergency_cat_levels),
      exacerbation_cat = create_cat_var(exacerbation_cat_levels),
      plan_n = create_cat_var(bin_levels),
      pefr_home = create_cat_var(bin_levels),
      
      # lung function
      pefr_pp = create_cat_var(pefr_levels),
      ppFEV1 = create_cat_var(fev1_levels),
      
      # allergies
      anaphylaxis = create_cat_var(comorb3_levels),
      angioedema_or_urticaria = create_cat_var(comorb2_levels),
      conjunctivitis = create_cat_var(comorb3_levels),
      eczema  = create_cat_var(comorb3_levels),
      rhinitis = create_cat_var(comorb3_levels),
      drug_allergy = create_cat_var(comorb2_levels),
      food_allergy = create_cat_var(comorb2_levels),
      other_allergy = create_cat_var(comorb2_levels),
      oral_antihistamine = create_cat_var(comorb2_levels),
      
      # comorbidities
      Anxiety  = create_cat_var(comorb3_levels),
      Depression = create_cat_var(comorb3_levels),
      Diabetes = create_cat_var(comorb2_levels),
      GORD = create_cat_var(comorb3_levels),
      Nasal_polyps = create_cat_var(comorb2_levels),
      cci = rpois(n = n(), lambda = 3), # not used in clustering but used for tsne plots
      
      # comedications
      beta_blocker = create_cat_var(med2_levels),
      NSAID = create_cat_var(med2_levels),
      paracetamol = create_cat_var(med2_levels),
      statin = create_cat_var(med2_levels),
      
      # other
      URTI_cat = create_cat_var(infection_levels),
      LRTI_cat = create_cat_var(infection_levels),
      influenza_vaccination = create_cat_var(bin_levels),
      
    )
  
  # which ones still to do?
  dummy_vars <- names(dummy_data)
  todo <- select_features[!(select_features %in% dummy_vars)]
  stopifnot(purrr::is_empty(todo))
  
  return(dummy_data)
  
}

set.seed(seed)

main_dummy_data <- bind_rows(
  create_dummy_data(index_date = "2016-01-01", n = 2*parameters[["dataset_size"]]),
  create_dummy_data(index_date = "2017-01-01", n = parameters[["dataset_size"]]),
  create_dummy_data(index_date = "2018-01-01", n = parameters[["dataset_size"]])
)

validation_dummy_data <- bind_rows(
  create_dummy_data(index_date = "2016-01-01", n = parameters[["dataset_size"]]),
  create_dummy_data(index_date = "2017-01-01", n = parameters[["dataset_size"]]),
  create_dummy_data(index_date = "2018-01-01", n = parameters[["dataset_size"]])
)

readr::write_rds(main_dummy_data, here::here("analysis", "data", "main_dummy_data.RDS"), compress = "gz")

readr::write_rds(validation_dummy_data, here::here("analysis", "data", "validation_dummy_data.RDS"), compress = "gz")
