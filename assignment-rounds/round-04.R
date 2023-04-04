devtools::load_all()

library(dplyr)

paths <- list(
    apps = file.path(
        "Z:/FSC - NPower/data-2022-2023/randomization",
        "Cohort 4 Applicants February 2023",
        "NP Applicants 2023-02-03.csv"
    ),
    offers = file.path(
        "Z:/FSC - NPower/data-2022-2023/randomization",
        "Cohort 4 Applicants February 2023",
        "NP Offers 2023-02-03.csv"
    )
)

applicants <- read_applicant_file(paths$apps)
offers <- readr::read_csv(paths$offers)

# Confirm that read_applicant_file is correctly determinimg eligibility: GOOD

araw <- readr::read_csv(paths$apps)

atest <- araw |>
    janitor::clean_names() |>
    dplyr::select(
        applicant_id = lead_id,
        rct_eligible
    )

atest |>
    left_join(applicants) |>
    count(assignment_eligible, rct_eligible)

# Confirm agreement between offers summary and applicant data
# - qualified applicants: GOOD
# - assignment-eligible applicants: GOOD

# count of qualified applicants
applicants |>
    count(prov, program_short)

# count of assignment-eligible applicants
# | ON - JITA | 279 - 64 - 6 =  209 |
# | AB - JITA |  95 -  2 - 1 =   92 |
applicants |>
    count(prov, program_short, assignment_eligible)

# Check gender balance of assignment-eligible applicants
# - Priority gender group is over-represented in both provs
# - AB's imbalance exceeds the stratification threshold,
#   but its overall treatment probability is > .8,
#   so stratification doesn't trigger.
applicants |>
    filter(assignment_eligible == 1) |>
    group_by(prov, program_short) |>
    summarize(pg_rep = mean(priority_gender_group))

params <- get_latest_stratification_parameters()

on_jita <- applicants |>
    filter(prov == "ON", program_short == "JITA", assignment_eligible == 1)

on_jita_offers <- calc_stratified_offers(on_jita, params, 155)
on_jita_rep <- on_jita |>
    group_by(priority_gender_group) |>
    summarize(
        n_assignments = n()
    ) |>
    left_join(on_jita_offers) |>
    mutate(p_trt = n_offers / n_assignments)

ab_jita <- applicants |>
    filter(prov == "AB", program_short == "JITA", assignment_eligible == 1)

ab_jita_offers <- calc_stratified_offers(ab_jita, params, 76)
ab_jita_rep <- ab_jita |>
    group_by(priority_gender_group) |>
    summarize(
        n_assignments = n()
    ) |>
    left_join(ab_jita_offers) |>
    mutate(p_trt = n_offers / n_assignments)

on_jita_rep
ab_jita_rep

# The seed value 1738 is inspired by rapper Fetty Wap's signature
# ad-lib, which itself refers to Remy Martin's 1738 congac.

assign_to_condition(applicants, n_offers_by_program_prov = offers, seed = 1738)