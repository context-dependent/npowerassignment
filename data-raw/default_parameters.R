## code to prepare `default_parameters` dataset goes here

default_parameters <- list(
    minimum_list_size = 5000, 
    overall_treatment_probability = 0.5,
    minimum_treatment_probability = 0.2, 
    maximum_treatment_probability = 0.7, 
    block_size = 20,
    rep_target_gender_diversity = 0.5, 
    seed = 489, 
    eligible_programs = "Junior IT Analyst Program (JITA)", 
    eligible_locations = "Greater Toronto Area - GTA, ON"
)

usethis::use_data(default_parameters, overwrite = TRUE)
