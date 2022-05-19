## code to prepare `default_parameters` dataset goes here

default_parameters <- list(
    minimum_treatment_probability = 0.2, 
    maximum_treatment_probability = 0.8, 
    rep_target_gender_diversity = 0.5, 
    rep_minimum_gender_diversity = 0.4, 
    eligible_programs = "Junior IT Analyst Program (JITA);Junior Data Analyst Program (JDA)", 
    eligible_locations = "Greater Toronto Area - GTA, ON"
)

usethis::use_data(default_parameters, overwrite = TRUE)
