# Assignment code for cohort assigned on 2022-08-29
library(readxl)
library(npowerassignment)

# Examine raw data to check for required changes
# Changes to raw data:
# - Created column to track previous randomization
# - Saved as two CSVs, Applicants and Offers
# - Separated prov and program_short columns in offers
data_dir <- "Z:/FSC - NPower/data-2022/transfer/Cohort Applicants August 2022"

data_path <- fs::dir_ls(data_dir)[1]

dat_raw <- read_excel(data_path, sheet = "NPower applicant data for rando")

offer_nums <- read_excel(data_path, sheet = "Number to be randomized in")

locations <- unique(dat_raw$`Program Offered in:`)

locations

update_stratification_parameters(list(eligible_locations = "Greater Toronto Area - GTA, ON;Calgary, AB"))

params <- get_latest_stratification_parameters()

params

# Examine gender distribution in eligible applicants

dat_apps <- read_applicant_file("Z:/FSC - NPower/data-2022/transfer/Cohort Applicants August 2022/NP Applicants 2022-08-29.csv")

dat_apps |>
    dplyr::group_by(program_short, prov) |>
    dplyr::summarize(pg_rep = mean(priority_gender_group))

