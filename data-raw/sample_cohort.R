## code to prepare `sample_cohort` dataset goes here

sample_cohort <- readr::read_csv("data-raw/sample-report.csv") 
usethis::use_data(sample_cohort, overwrite = TRUE)
