sanitise_categories <- function(.data, var_names) {
  .data %>%
    mutate_at(vars(c(var_names)), list(~ case_when(. %in% 'F' ~ 'Female',
                                                   . %in% 'M' ~ 'Male',
                                                   . %in% 'Asian British' ~ .,
                                                   . %in% '[18,34]' ~ '18-34 years', 
                                                   . %in% '(34,46]' ~ '35-46 years',
                                                   . %in% '(46,56]' ~ '47-56 years',
                                                   . %in% '(56,69]' ~ '57-69 years',
                                                   . %in% '(69,100]' ~ '70+ years',
                                                   . %in% '>=18' ~ str_c(., ' years'),
                                                   . %in% '<18' ~ str_c(., ' years'),
                                                   . %in% '<=0.4' ~ '<=400 cells per microlitre',
                                                   . %in% '>0.4' ~ '>400 cells per microlitre',
                                                   . %in% 'Median (IQR)' ~ .,
                                                   . %in% '<20' ~ str_c(., ' kg/m^2'),
                                                   . %in% '20-24' ~ str_c(., ' kg/m^2'),
                                                   . %in% '25-29' ~ str_c(., ' kg/m^2'),
                                                   . %in% '30-39' ~ str_c(., ' kg/m^2'),
                                                   . %in% '40+' ~ str_c(., ' kg/m^2'),
                                                   TRUE ~ str_to_sentence(.))))
}
