assign_cohort <- function(applicants) {
    stratification_parameters <- get_latest_stratification_parameters()
    eligible_applicants <- extract_eligible_applicants(applicants, stratification_parameters)
    treatment_probabilities <- calculate_treatment_probabilities(eligible_applicants, stratification_parameters)
    assignment_lists <- get_latest_assignment_bundle()

    if(treatment_probabilities$same_list) {
        new_assignments <- assign_applicant_group(
            eligible_applicants, 
            treatment_probabilities$priority_gender, 
            assignment_lists
        )
    } else {
        pg_assignments <- assign_applicant_group(
            eligible_applicants |> dplyr::filter(priority_gender_group == 1),
            treatment_probabilities$priority_gender, 
            assignment_lists
        )

        non_pg_assignments <- assign_applicant_group(
            eligible_applicants |> dplyr::filter(priority_gender_group == 0), 
            treatment_probabilities$non_priority_gender, 
            assignment_lists
        )

        new_assignments <- bind_rows(pg_assignments, non_pg_assignments)
    }

    return(new_assignments)

}

extract_eligible_applicants <- function(
    applicants, 
    stratification_parameters
) {

    eligible_locations <- stratification_parameters$eligible_locations  |> 
        stringr::str_split(";") |>
        unlist()
    eligible_programs <- stratification_parameters$eligible_programs  |> 
        stringr::str_split(";") |>
        unlist()

    eligible_applicants <- applicants  |>
        dplyr::filter(
            location %in% eligible_locations, 
            program %in% eligible_programs, 
            assignment_eligible == 1
        )
    
    return(eligible_applicants)

}

calculate_treatment_probabilities <- function(
    eligible_applicants, 
    stratification_parameters
) {
    trt_lists <- seq(
            stratification_parameters$minimum_treatment_probability,
            stratification_parameters$maximum_treatment_probability,
            1 / stratification_parameters$block_size
        )

    trt <- stratification_parameters$overall_treatment_probability

    pg_rep_target <- stratification_parameters$rep_target_gender_diversity
    pg_rep_sample <- mean(eligible_applicants$priority_gender_group)
    pg_trt_unbounded <- (pg_rep_target * trt) / pg_rep_sample
    pg_trt_list <- trt_lists[which.min(abs(trt_lists - pg_trt_unbounded))]
    pg_rep_cohort <- (pg_trt_list * pg_rep_sample) / trt
    
    non_pg_rep_target <- 1 - pg_rep_cohort
    non_pg_rep_sample <- 1 - pg_rep_sample
    non_pg_trt_unbounded <- (non_pg_rep_target * trt) / non_pg_rep_sample
    non_pg_trt_list <- trt_lists[which.min(abs(trt_lists - non_pg_trt_unbounded))]

    res <- list(
        same_list = pg_trt_list == non_pg_trt_list, 
        priority_gender = glue::glue("block-{stratification_parameters$block_size}_p-trt-{pg_trt_list * 100}"), 
        non_priority_gender = glue::glue("block-{stratification_parameters$block_size}_p-trt-{non_pg_trt_list * 100}")
    ) 

    return(res)

}

assign_applicant_group <- function(applicant_group, assignment_list_name, assignment_list_bundle) {

    activity_id <- log_activity("Assign Applicant Group")
    used_assignments_sheet <- get_used_assignments()
    used_assignments_data <- used_assignments_sheet |>
        googlesheets4::read_sheet()
    assignment_list <- googlesheets4::read_sheet(assignment_list_bundle, sheet = assignment_list_name)
    assignment_list_bundle_id <- googlesheets4::read_sheet(assignment_list_bundle, sheet = "meta")[["assignment_list_bundle_id"]][1]
    assignment_list_bundle_name <- assignment_list_bundle$name

    if(nrow(used_assignments_data) > 0) {
        fresh_assignments <- assignment_list |>
            dplyr::anti_join(used_assignments_data, by = "assignment_id")
    } else { 
        fresh_assignments <- assignment_list
    }
    
    assignments_on_deck <- fresh_assignments[1:nrow(applicant_group), ]
    assigned_applicants <- cbind(applicant_group, assignments_on_deck) |>
        dplyr::select(-matches("activity_id")) |>
        dplyr::transmute(
            assignment_list_bundle_id = assignment_list_bundle_id, 
            assignment_list_bundle_name = assignment_list_bundle_name, 
            assignment_list_name = assignment_list_name, 
            applicant_id, 
            assignment_id, 
            assignment_label, 
            activity_id = activity_id,
        )

    
    googlesheets4::sheet_append(used_assignments, assigned_applicants, sheet = 1)

    return(assigned_applicants)

}
