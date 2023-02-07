# Assignment Operations for 2022-12-05

devtools::load_all()

# Read Data
# =========

d <- read_applicant_file()


applicants <- read_applicant_file("Z:/FSC - NPower/data-2022-2023/randomization/Cohort 3 Applicants December 2022/NP Applicants 2022-12-01.csv")


# Issues
# ------

app_dat <- readr::read_csv("Z:/FSC - NPower/data-2022-2023/randomization/Cohort 3 Applicants December 2022/NP Applicants 2022-12-01.csv") |>
    janitor::clean_names()

# 1. Offers in new format : Converted to prev manually
# 2. Problem determining previous randomization :
#    - Previously Randomized no longer a column,
#    - Now the info is compounded in the RCT Eligible field
#    - Assignment date has been added, so maybe that's a convenient proxy
#    - I think I can just use rct_eligible == "Yes", tbh.
#      There were initially a pile of missing rct_eligible but that
#      seems to just be a saving issue

app_dat |>
    dplyr::count(rct_eligible)



dat_offers <- readr::read_csv("Z:/FSC - NPower/data-2022-2023/randomization/Cohort 3 Applicants December 2022/NP Offers 2022-12-01.csv")
dat_offers

library(dplyr)

applicants |>
    dplyr::filter(assignment_eligible == 1) |>
    dplyr::group_by(prov, program_short) |>
    dplyr::summarize(
        n = n(),
        pg_rep = mean(priority_gender_group)
    )

# Priority Gender Group is over-represented in each cohort.
# None of the cohorts are imbalanced enough to suggest stratification.

params <- get_latest_stratification_parameters()

eligible_applicants <- applicants |>
    extract_eligible_applicants(stratification_parameters = params)

dat_assignment <- eligible_applicants |>
    dplyr::group_nest(program, prov, program_short, .key = "program_cohorts") |>
    dplyr::mutate(
        assignment_date = Sys.Date(),
        n_applicants = program_cohorts |>
            purrr::map_dbl(nrow),
        pg_rep = program_cohorts |>
            purrr::map_dbl(~ mean(.x$priority_gender_group)),
        npg_rep = 1 - pg_rep
    ) |>
    dplyr::left_join(dat_offers) |>
    dplyr::mutate(trt_ovr = n_offers / n_applicants) |>
    dplyr::mutate(
        assignments = purrr::map2(
            program_cohorts,
            n_offers,
            ~ assign_program_cohort_exact(.x, .y, params)
        )
    )


dat_assignment |>
    dplyr::select(prov, program_short, assignment_date, assignments) |>
    tidyr::unnest(assignments) |>
    dplyr::group_by(prov, program_short, priority_gender_group) |>
    dplyr::summarize(p_trt = mean(trt))

# This set up appears to be working correctly.
# The system is appropriately declining to stratify
# the applicant cohorts

# The seed value 1337 is the representation of the word "LEET"
# in the eponymous LEET SPEAK, a variant of written english
# in which each letter of the alphabet is replaced with
# some combination of non-alphabetic symbols.

assign_to_condition(
    applicants,
    n_offers_by_program_prov = dat_offers,
    seed = 1337
)