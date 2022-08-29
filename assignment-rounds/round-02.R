# Assignment code for cohort assigned on 2022-08-29
library(readxl)
library(npowerassignment)

# Examine raw data to check for required changes
data_dir <- "Z:/FSC - NPower/data-2022/transfer/Cohort Applicants August 2022"

data_path <- fs::dir_ls(data_dir)[1]

dat_raw <- read_excel(data_path, sheet = "NPower applicant data for rando")

offer_nums <- read_excel(data_path, sheet = "Number to be randomized in")

locations <- unique(dat_raw$`Program Offered in:`)

update_stratification_parameters(list(eligible_locations = "Greater Toronto Area - GTA, ON;Calgary, AB"))