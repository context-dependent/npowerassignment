pacman::p_load(
    "tidyverse"
)

devtools::load_all()


applicants <- read_applicant_file("Z:/FSC - NPower/data-2022/transfer/Cohort Applicants May 2022/NPower May2022 Cohort Applicants RCT 2022-05-06.csv")

test_assignments <- assign_applicant_batch_exact(applicants, n_offers_by_program = list(JITA = 217, JDA = 166), seed = 489, browse = TRUE)



