library(googledrive)
library(googlesheets4)
library(tidyverse)
devtools::load_all()

assignments <- drive_find("assignments", type = "spreadsheet") |>
    gs4_get() |>
    read_sheet()

params <- get_latest_stratification_parameters()

used_assignments <- get_used_assignments() |> read_sheet()

applicants <- read_applicant_file("Z:/FSC - NPower/data-2022/transfer/Cohort Applicants May 2022/NPower May2022 Cohort Applicants RCT 2022-05-06.csv")

eligible_applicants <- extract_eligible_applicants(applicants, params, used_assignments_data  |> filter(FALSE))

eligible_applicants_jita <- eligible_applicants |>
    filter(program |> str_detect("JITA"))


applicants_with_assignments <- applicants |>
    left_join(assignments) |>
    filter(assignment_eligible == 1 & is.na(assignment_date))

applicants_with_assignments |>
    count(program)

eas <- extract_eligible_applicants(applicants, params, used_assignments)

eas |> count(program)
