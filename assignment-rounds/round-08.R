devtools::load_all()
bpconnect::vpn_start()


library(dplyr)

paths <- list(
    apps = file.path(
        "Z:/FSC - NPower/data-2022-2023/randomization",
        "Cohort 9 Applicants August 21 2023",
        "NP Applicants 2023-30-07.csv"
    ),
    offers = file.path(
        "Z:/FSC - NPower/data-2022-2023/randomization",
        "Cohort 9 Applicants August 21 2023",
        "NP Offers 2023-07-30.csv"
    )
)

applicants <- read_applicant_file(paths$apps) |>
    filter(!is.na(applicant_id))
offers <- readr::read_csv(paths$offers)

# Confirm that read_applicant_file is correctly determinimg eligibility: GOOD

araw <- readr::read_csv(paths$apps)

anames <- araw |>
    janitor::clean_names()

names(anames) <- stringr::str_remove(names(anames), "_c$")

anames |>
    count(rct_eligible)

atest <- anames |>
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
applicants |>
    count(prov, program_short, assignment_eligible)

# Check gender balance of assignment-eligible applicants
# - Priority gender group is slightly overrepresented in AB,
#   roughly even in ON (49%)
# - AB is triggering stratification, but such that pg (over-repped)
#   has a higher treatment probability.
applicants |>
    filter(assignment_eligible == 1) |>
    group_by(prov, program_short) |>
    summarize(pg_rep = mean(priority_gender_group))

params <- get_latest_stratification_parameters()

on_jita <- applicants |>
    filter(prov == "ON", program_short == "JITA", assignment_eligible == 1)

on_jita_offers <- calc_stratified_offers(on_jita, params, 195)
on_jita_rep <- on_jita |>
    group_by(priority_gender_group) |>
    summarize(
        n_assignments = n()
    ) |>
    left_join(on_jita_offers) |>
    mutate(
        n_ctrl = n_assignments - n_offers,
        p_trt = n_offers / n_assignments
    )

ab_jita <- applicants |>
    filter(prov == "AB", program_short == "JITA", assignment_eligible == 1)

ab_jita_offers <- calc_stratified_offers(ab_jita, params, 66)
ab_jita_rep <- ab_jita |>
    group_by(priority_gender_group) |>
    summarize(
        n_assignments = n()
    ) |>
    left_join(ab_jita_offers) |>
    mutate(
        n_ctrl = n_assignments - n_offers,
        p_trt = n_offers / n_assignments
    )

on_jita_rep
ab_jita_rep

# Find the eligible applicant who wasn't assigned to the control group
# - were they assigned to the treatment group? no, the treatment group is the correct size

used_assignments <- get_used_assignments() |>
    googlesheets4::read_sheet()

used_assignments

applicants |>
    filter(assignment_eligible == 1) |>
    select(applicant_id) |>
    left_join(used_assignments) |>
    count(assignment_date)

# They were previously assigned!

applicants |>
    filter(assignment_eligible == 1) |>
    select(applicant_id) |>
    left_join(used_assignments) |>
    arrange(assignment_date) |>
    select(applicant_id, trt)



# The seed value 622401 is the sku number for canada dry gingerale

assign_to_condition(applicants, n_offers_by_program_prov = offers, seed = 622401)
