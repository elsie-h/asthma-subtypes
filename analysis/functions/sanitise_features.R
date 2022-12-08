sanitise_features <- function(.data, var_names) {
  .data %>%
    mutate_at(vars(c(var_names)), list(~ case_when(. %in% 'sex_coded' ~ 'Sex',
                                                  . %in% 'index_age_q' ~ 'Index age quintile',
                                                  . %in% 'age_onset_cat' ~ 'Age of diagnosis',
                                                  . %in% 'eos' ~ 'Blood eosinophil count',
                                                  . %in% 'emergency_cat'~ 'Emergency events',
                                                  . %in% 'exacerbation_cat' ~ 'Exacerbation',
                                                  . %in% 'ar_cat' ~ 'Asthma reviews',
                                                  . %in% 'influenza vaccination' ~ 'Flu vaccine',
                                                  . %in% 'pefr_pp' ~ 'ppPEF',
                                                  . %in% 'plan_n' ~ 'Asthma action plan',
                                                  . %in% 'pefr_home' ~ 'PEF home monitor',
                                                  . %in% 'cci_cat' ~ 'CCI',
                                                  . %in% 'Nasal_polyps' ~ 'Nasal polyps',
                                                  . %in% 'Cardiovascular_disease' ~ 'CVD',
                                                  . %in% 'Chronic_cardiac_disease' ~ 'CCD',
                                                  . %in% 'Imd decile' ~ 'IMD decile',
                                                  . %in% 'methylxanthine_cat' ~ 'Methylxanthine',
                                                  . %in% 'FEV1FVC' ~ 'FEV1/FVC',
                                                  . %in% 'spacer_cat' ~ 'Spacer',
                                                  str_detect(., '[A-Z]+') ~ str_remove(., '_cat'),
                                                  TRUE ~ str_to_sentence(str_replace_all(., '_', ' ')))))
}
