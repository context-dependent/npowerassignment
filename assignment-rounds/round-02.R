# Assignment code for cohort assigned on 2022-08-29
devtools::load_all()

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

params <- get_latest_stratification_parameters()
dat_apps <- read_applicant_file("Z:/FSC - NPower/data-2022/transfer/Cohort Applicants August 2022/NP Applicants 2022-08-29.csv")

dat_apps |>
    dplyr::group_by(program_short, prov) |>
    dplyr::summarize(pg_rep = mean(priority_gender_group))

dat_elapps <- extract_eligible_applicants(dat_apps, params)

dat_offers <- readr::read_csv("Z:/FSC - NPower/data-2022/transfer/Cohort Applicants August 2022/NP Offers 2022-08-29.csv")

dat_assign_eg <- prep_assignment_frame(dat_elapps, n_offers_by_program_prov = dat_offers, seed = 489, activity_id = "TEST")


prep_assignment_frame <- function(
    applicants, 
    n_offers_by_program = list(), 
    n_offers_by_program_prov = NULL, 
    seed, activity_id, ignore_existing = FALSE, browse = FALSE) {

    set.seed(seed)

    if(browse) {
        browser()
    }

    params <- get_latest_stratification_parameters()
    wb_assignments <- get_used_assignments()
    
    if(!is.null(wb_assignments)) {
        dat_assignments <- wb_assignments |> googlesheets4::read_sheet()
    } else {
        dat_assignments <- NULL
    }
    
    eligible_applicants <- extract_eligible_applicants(applicants, params, dat_assignments, ignore_existing = ignore_existing)

    if(is.null(n_offers_by_program_prov)) {
        program_offers <- tibble::enframe(n_offers_by_program) |>
            dplyr::transmute(
                program_short = name, 
                n_offers = unlist(value)
            )
    } else {
       program_offers <- n_offers_by_program_prov
    }

    assignment_ready <- eligible_applicants |>
        dplyr::group_nest(program, prov, program_short, .key = "program_cohorts") |>
        dplyr::mutate(
            assignment_date = Sys.Date(),
            n_applicants = program_cohorts |>
                purrr::map_dbl(nrow),
            pg_rep = program_cohorts  |>
                purrr::map_dbl(~ mean(.x$priority_gender_group)),
            npg_rep = 1 - pg_rep
        ) |>
        dplyr::left_join(program_offers)  |>
        dplyr::mutate(trt_ovr = n_offers / n_applicants)

    return(assignment_ready)
}

assign_program_cohort_exact(dat_assign_eg$program_cohorts[[1]], dat_assign_eg$n_offers[1], params)
assign_program_cohort_exact(dat_assign_eg$program_cohorts[[2]], dat_assign_eg$n_offers[2], params)
assign_program_cohort_exact(dat_assign_eg$program_cohorts[[3]], dat_assign_eg$n_offers[3], params)
assign_program_cohort_exact(dat_assign_eg$program_cohorts[[4]], dat_assign_eg$n_offers[4], params)


calc_stratified_offers(dat_assign_eg$program_cohorts[[3]], params, dat_assign_eg$n_offers[3])
