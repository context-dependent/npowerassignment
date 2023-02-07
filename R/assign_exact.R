#' Assign a table of applicants imported with read_applicant_file()
#' 
#' @param n_offers_by_program
#' Controls the program-specific number of offers for the cohort of applicants
#' being assigned, in the form `list(<program_short> = <number_of_offers>)`. 
#' For example, in the first assignment round, we were required to assign 217
#' JITA applicants and 155 JDA applicants to the treatment group. In this case, 
#' `n_offers_by_program = list(JITA = 217, JDA = 155)`
#' 
#' @export
<<<<<<< Updated upstream
assign_to_condition <- function(applicants, n_offers_by_program = list(), n_offers_by_program_prov = NULL, seed, browse = FALSE) {

=======
assign_to_condition <- function(applicants,
                                n_offers_by_program = list(),
                                n_offers_by_program_prov = NULL,
                                seed,
                                browse = FALSE) {
>>>>>>> Stashed changes
    activity_id <- log_activity("Assign Applicant Batch")
    new_assignments <- assign_applicant_batch_exact(
        applicants,
        n_offers_by_program,
        n_offers_by_program_prov,
        seed,
        activity_id,
        browse = browse
    )

<<<<<<< Updated upstream
    params <- get_latest_stratification_parameters()
    new_assignments <- assign_applicant_batch_exact(applicants, n_offers_by_program, n_offers_by_program_prov, seed, activity_id, browse = browse)
    record_assignments_exact <- record_assignments_exact(applicants, new_assignments, seed, activity_id)

}

#' @export
record_assignments_exact <- function(applicants, new_assignments, seed, activity_id) {

=======
    record_assignments_exact <- record_assignments_exact(
        applicants,
        new_assignments,
        seed,
        activity_id
    )
}

#' @export
record_assignments_exact <- function(applicants,
                                     new_assignments,
                                     seed,
                                     activity_id) {
>>>>>>> Stashed changes
    applicant_summary_table <- applicants |>
        dplyr::group_by(program) |>
        dplyr::summarize(
            n_applicants = dplyr::n(), 
            n_eligible = sum(applicant_id %in% new_assignments)
        ) |>
        dplyr::mutate(
            n_ineligible = n_applicants - n_eligible
        ) |>
        dplyr::select(
            program, 
            n_applicants, 
            n_ineligible, 
            n_eligible
        )
    
    assignment_summary_table <- new_assignments |>
        dplyr::group_by(program, program_short, assignment_date) |>
        dplyr::summarize(
            n_eligible_priority_gender = sum(priority_gender_group), 
            p_eligible_priority_gender = sum(priority_gender_group) / dplyr::n(),
            n_offers = sum(trt),
            n_offers_priority_gender = sum(trt == 1 & priority_gender_group == 1), 
            n_offers_non_priority_gender = sum(trt == 1 & priority_gender_group == 0),
            treatment_probability = mean(trt),
            treatment_probability_priority_gender = mean(trt[priority_gender_group == 1]), 
            treatment_probability_non_priority_gender = mean(trt[priority_gender_group == 0])
        ) |>
        dplyr::mutate(
            activity_id = activity_id, 
            seed = seed
        )
    
    output <- applicant_summary_table |>
        dplyr::left_join(assignment_summary_table)

    output |> write_or_append("assignment-log")
    new_assignments |> write_or_append("assignments")

    invisible()
}

#' @export
<<<<<<< Updated upstream
assign_applicant_batch_exact <- function(applicants, n_offers_by_program = list(), n_offers_by_program_prov = NULL, seed, activity_id, ignore_existing = FALSE, browse = FALSE) {

=======
assign_applicant_batch_exact <- function(applicants,
                                         n_offers_by_program = list(),
                                         n_offers_by_program_prov = NULL,
                                         seed,
                                         activity_id,
                                         ignore_existing = FALSE,
                                         browse = FALSE) {
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
    eligible_applicants <- extract_eligible_applicants(applicants, params, dat_assignments, ignore_existing = ignore_existing)
    if(is.null(n_offers_by_program_prov)) {
=======
    eligible_applicants <- extract_eligible_applicants(
        applicants,
        params,
        dat_assignments,
        ignore_existing = ignore_existing
    )
    if (is.null(n_offers_by_program_prov)) {
>>>>>>> Stashed changes
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
        dplyr::mutate(assignment_date = Sys.Date()) |>
        dplyr::left_join(program_offers) 
    
    assignments <- assignment_ready |>
        dplyr::mutate(
            assignments = purrr::map2(
                program_cohorts, 
                n_offers, 
                function(x, y) {
                    assign_program_cohort_exact(x, y, params)
                }
            )
        ) |>
        dplyr::select(
            -c(program_cohorts, n_offers)
        ) |>
        tidyr::unnest(
            c(assignments)
        ) |>
        dplyr::mutate(activity_id = activity_id)

    return(assignments)
}

assign_program_cohort_exact <- function(program_cohort, n_offers_for_program, params, browse = FALSE) {
    if(browse) browser()
    stratified_offers <- calc_stratified_offers(program_cohort, params, n_offers_for_program)

    assignments <- program_cohort |>
        dplyr::group_nest(priority_gender_group, .key = "applicant_groups")  |>
        dplyr::left_join(stratified_offers) |>
        dplyr::mutate(
            assignments = purrr::map2(
                applicant_groups, 
                n_offers,
                assign_applicant_group_exact, 
                browse = browse
            )
        ) |>
        dplyr::select(priority_gender_group, assignments) |>
        tidyr::unnest(cols = c(assignments))

    return(assignments)

}


<<<<<<< Updated upstream
assign_applicant_group_exact <- function(applicant_group, n_offers_group, browse = FALSE) {

    if(browse) browser()
=======
assign_applicant_group_exact <- function(applicant_group,
                                         n_offers_group,
                                         browse = FALSE) {
    if (browse) browser()
>>>>>>> Stashed changes

    n_applicants <- nrow(applicant_group)
    n_control <- n_applicants - n_offers_group
    assignments <- tibble::tibble(
        trt = c(rep(1, n_offers_group), rep(0, n_control))) |>
        dplyr::mutate(
            assignment_label = dplyr::case_when(
                trt == 1 ~ "Treatment", 
                trt == 0 ~ "Control"
            ) |>
            forcats::fct_relevel(
                "Control", 
                "Treatment"
            ), 
            assignment_id = uuid::UUIDgenerate(n = dplyr::n())
        )

    assigned_applicants <- applicant_group |>
        dplyr::mutate(randomizer = runif(dplyr::n())) |>
        dplyr::arrange(randomizer) |>
        dplyr::bind_cols(assignments)

    return(assigned_applicants)

}

#' @export
calc_stratified_offers <- function(program_cohort, params, n_offers) {

    n_app <- nrow(program_cohort)
    n_app_pg <- sum(program_cohort$priority_gender_group)
    n_app_non_pg <- n_app - n_app_pg
    pg_rep_target <- params$rep_target_gender_diversity
    trt_max <- params$maximum_treatment_probability

    pg_rep <- n_app_pg / n_app
    npg_rep <- n_app_non_pg / n_app

    p_trt <- n_offers / n_app

    p_trt_pg <- (pg_rep_target * p_trt) / pg_rep
    n_trt_pg <- ceiling(p_trt_pg * n_app_pg)
    n_trt_non_pg <- n_offers - n_trt_pg
    p_trt_non_pg <- n_trt_non_pg / n_app_non_pg

    pg_rep_min <- 0.4
    p_trt_pg_min <- (pg_rep_min * p_trt) / pg_rep 

    if(abs(npg_rep - pg_rep) <= 0.20 | p_trt > 0.8) {
        # if the applicant group is closer to 50/50 than 60/40, 
        # or if the overall treatment probability is greater than 0.8, 
        # don't strat
        n_trt_pg <- ceiling(p_trt * n_app_pg)
        n_trt_non_pg <- n_offers - n_trt_pg
    } else if(p_trt_pg > trt_max) {
        # If the treatment probability for pg apps is over the max, 
        # we tweak it. 
        
        if(p_trt_pg > 1) {
            # If the treatment probability for pg apps is over 1, 
            # I guess it's oprah style? 
            if(p_trt_pg_min > 1) {
                # If the min treatment probability is also over 1, 
                # It's definitely oprah style
                p_trt_pg <- 1
                
            } else {
                # Otherwise, I guess just make it the minimum 
                p_trt_pg <- p_trt_pg_min
            }
        } else if(p_trt_non_pg > trt_max) {
            # If the treatment probabiltiy for non pg applicants is also over the max, 

        } else if(p_trt_pg_min > trt_max){
            # Otherwise, if the minimum treatment probability to achieve
            # the minimum level of pg representation is still greater 
            # than the maximum treatment probability, set the treatment 
            # probability for pg apps to the minimum value. 
            p_trt_pg <- p_trt_pg_min
        } else {
            # If the minimum treatment probability is, on the other hand
            # less than the maximum treatment probability, compromise
            # by taking the midpoint between the two values
            p_trt_pg <- mean(c(p_trt_pg_min, trt_max)) 
        }

        n_trt_pg <- ceiling(p_trt_pg * n_app_pg)
        n_trt_non_pg <- min(n_offers - n_trt_pg, n_app_non_pg)

    } 

    n_offers_by_gender <- tibble::tribble(
        ~ priority_gender_group,   ~ n_offers, 
                              0, n_trt_non_pg,
                              1,     n_trt_pg
    )

    if(sum(n_offers_by_gender$n_offers) < n_offers) {
        warning("Unable to provide requested number of offers")
    }

    return(n_offers_by_gender)
}

#' @export
extract_eligible_applicants <- function(
    applicants, 
    stratification_parameters, 
    used_assignments_data = NULL,
    ignore_existing = FALSE
) {

    eligible_locations <- stratification_parameters$eligible_locations  |> 
        stringr::str_split(";") |>
        unlist()
    eligible_programs <- stratification_parameters$eligible_programs  |> 
        stringr::str_split(";") |>
        unlist()

    fresh_applicants <- applicants  |>
        dplyr::filter(
            location %in% eligible_locations, 
            program %in% eligible_programs, 
            assignment_eligible == 1, 
        ) 

    if(ignore_existing) used_assignments_data <- NULL
    
    if(!is.null(used_assignments_data)) {
        fresh_applicants <- fresh_applicants |>
            dplyr::filter(
                ! (applicant_id %in% used_assignments_data$applicant_id)
            )
    }

    return(fresh_applicants)

}

#' Import and clean up a report file of applicants from NPower. 
#' @export
read_applicant_file <- function(path, quiet = FALSE) {

    applicants <- readr::read_csv(path) |>
        janitor::clean_names()

    res <- applicants  |>
        dplyr::transmute(
            applicant_id = lead_id, 
            priority_gender_group = as.numeric(gender_priority_group == "Yes"), 
            assignment_eligible = as.numeric(rct_eligible == "Yes" & previously_randomized == "No"), 
            location = program_offered_in, 
            prov = program_offered_in |> stringr::str_extract("ON|AB"),
<<<<<<< Updated upstream
            program = being_considered_for, 
            program_short = being_considered_for |> stringr::str_extract("[A-Z]{3}[A-Z]*")
=======
            program = being_considered_for,
            program_short = being_considered_for |>
                stringr::str_extract("[A-Z]{3}[A-Z]*")
>>>>>>> Stashed changes
        )

    return(res)

    
}