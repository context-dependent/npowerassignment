assign_exact <- function(applicants, n_offers_by_program = list(), seed) {

    activity_id <- log_activity("Assign Applicant Batch")

    params <- get_latest_stratification_parameters()
    new_assignments <- assign_applicant_batch_exact(eligible_applicants, n_offers_by_program, seed, activity_id)
    record_assignments_exact <- record_assignments_exact(applicants, new_assignments, seed, activity_id)

}

record_assignments_exact <- function(applicants, new_assignments, seed, activity_id) {

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

    assignment_log |> write_or_append("assignment-log")
    new_assignment |> write_or_append("assignments")

    invisible()
}

assign_applicant_batch_exact <- function(applicants, n_offers_by_program = list(), seed, activity_id, browse = FALSE) {

    set.seed(seed)

    if(browse) {
        browser()
    }

    params <- get_latest_stratification_parameters()
    wb_assignments <- get_used_assignments()
    dat_assignments <- wb_assignments |> googlesheets4::read_sheet()
    eligible_applicants <- extract_eligible_applicants(applicants, params, dat_assignments)
    program_offers <- tibble::enframe(n_offers_by_program) |>
        dplyr::transmute(
            program_short = name, 
            n_offers = unlist(value)
        )

    assignments <- eligible_applicants |>
        dplyr::group_nest(program, program_short, .key = "program_cohorts") |>
        dplyr::mutate(assignment_date = Sys.Date()) |>
        dplyr::left_join(program_offers) |>
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


assign_applicant_group_exact <- function(applicant_group, n_offers_group, browse = FALSE) {

    if(browse) browser()

    n_applicants <- nrow(applicant_group)
    n_control <- n_applicants - n_offers_group
    assignments <- tibble::tibble(
        trt = c(rep(1, n_offers_group), rep(0, n_control))) |>
        dplyr::mutate(
            assignment_label = case_when(
                trt == 1 ~ "Treatment", 
                trt == 0 ~ "Control"
            ) |>
            fct_relevel(
                "Control", 
                "Treatment"
            ), 
            assignment_id = uuid::UUIDgenerate(n = n())
        )

    assigned_applicants <- applicant_group |>
        dplyr::mutate(randomizer = runif(n())) |>
        dplyr::arrange(randomizer) |>
        dplyr::bind_cols(assignments)

    return(assigned_applicants)

}

calc_stratified_offers <- function(program_cohort, params, n_offers) {

    n_app <- nrow(program_cohort)
    n_app_pg <- sum(program_cohort$priority_gender_group)
    n_app_non_pg <- n_app - n_app_pg
    pg_rep_target <- params$rep_target_gender_diversity
    trt_max <- params$maximum_treatment_probability

    pg_rep <- n_app_pg / n_app
    p_trt <- n_offers / n_app

    p_trt_pg <- (pg_rep_target * p_trt) / pg_rep
    n_trt_pg <- ceiling(p_trt_pg * n_app_pg)
    n_trt_non_pg <- n_offers - n_trt_pg
    p_trt_non_pg <- n_trt_non_pg / n_app_non_pg

    pg_rep_min <- 0.4
    p_trt_pg_min <- (pg_rep_min * p_trt) / pg_rep 

    if(p_trt_pg > trt_max) {
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
            # fuck it, we're sauced. 

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


extract_eligible_applicants <- function(
    applicants, 
    stratification_parameters, 
    used_assignments_data
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
            ! (applicant_id %in% used_assignments_data$applicant_id)
        ) 

    return(fresh_applicants)

}