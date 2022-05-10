#' Assign a batch of applicants 
#' 
#' Take a batch of raw applicant data, process it for assignment
#' Draw assignments, and upload them to the google drive. 
#' 
#' @export
assign_cohort <- function(applicants) {

    stratification_parameters <- get_latest_stratification_parameters()
    used_assignments_sheet <- get_used_assignments()
    used_assignments_data <- used_assignments_sheet |>
        googlesheets4::read_sheet()
    eligible_applicants <- extract_eligible_applicants(applicants, stratification_parameters, used_assignments_data)

    if(nrow(eligible_applicants) == 0) {
        message("No new eligible applicants to assign")
        return(eligible_applicants)
    }

    treatment_probabilities <- calculate_treatment_probabilities(eligible_applicants, stratification_parameters)
    assignment_lists <- get_latest_assignment_bundle()



    if(treatment_probabilities$same_list) {
        new_assignments <- assign_applicant_group(
            eligible_applicants, 
            treatment_probabilities$priority_gender, 
            assignment_lists, 
            used_assignments_sheet, 
            used_assignments_data
        )
    } else {
        pg_assignments <- assign_applicant_group(
            eligible_applicants |> dplyr::filter(priority_gender_group == 1),
            treatment_probabilities$priority_gender, 
            assignment_lists, 
            used_assignments_sheet, 
            used_assignments_data
        )

        non_pg_assignments <- assign_applicant_group(
            eligible_applicants |> dplyr::filter(priority_gender_group == 0), 
            treatment_probabilities$non_priority_gender, 
            assignment_lists, 
            used_assignments_sheet, 
            used_assignments_data
        )

        new_assignments <- dplyr::bind_rows(pg_assignments, non_pg_assignments)
    }

    update_assignment_log(new_assignments, treatment_probabilities)

    return(new_assignments)

}


update_assignment_log <- function(new_assignments, treatment_probabilities) {

    cohort_summary <- new_assignments |>
        dplyr::group_by(assignment_list_bundle_id, assignment_list_bundle_name, assignment_date, activity_id) |>
        dplyr::summarize(
            n_assigned = dplyr::n(), 
            n_control = sum(assignment_label == "Control"), 
            n_treatment = sum(assignment_label == "Treatment"), 
            n_treatment_pg = sum(assignment_label == "Treatment" & priority_gender_group == 1), 
            
        ) |>

        dplyr::ungroup() |>

        dplyr::mutate(, 
            treatment_probability_pg = treatment_probabilities$priority_gender_num, 
            treatment_probability_npg = treatment_probabilities$non_priority_gender_num,
            rep_treatment_pg = n_treatment_pg / n_treatment
        ) |>

        dplyr::select(
            assignment_list_bundle_id,
            assignment_list_bundle_name,
            treatment_probability_pg, 
            treatment_probability_npg, 
            assignment_date, 
            n_assigned,
            n_control,  
            n_treatment, 
            n_treatment_pg, 
            rep_treatment_pg,
            activity_id
        )

    wb_assignment_log <- googledrive::drive_find("assignment-log") |>
        googlesheets4::gs4_get()  |>
        googlesheets4::sheet_append(cohort_summary)

    invisible()
    
}

read_applicant_file <- function(path) {

    applicants <- readr::read_csv(path) |>
        janitor::clean_names()

    res <- applicants  |>
        dplyr::transmute(
            applicant_id = lead_id, 
            priority_gender_group = as.numeric(gender_priority_group == "Yes"), 
            assignment_eligible = as.numeric(rct_eligible == "Yes"), 
            location = program_offered_in, 
            program = being_considered_for
        ) |>
        dplyr::arrange(
            applicant_id
        )

    return(res)

    
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
        non_priority_gender = glue::glue("block-{stratification_parameters$block_size}_p-trt-{non_pg_trt_list * 100}"), 
        priority_gender_num = pg_trt_list, 
        non_priority_gender_num = non_pg_trt_list
    ) 

    return(res)

}

assign_applicant_group <- function(
    applicant_group, 
    assignment_list_name, 
    assignment_list_bundle, 
    used_assignments_sheet, 
    used_assignments_data
) {

    activity_id <- log_activity("Assign Applicant Group")
    
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
            priority_gender_group, 
            assignment_id, 
            assignment_date = lubridate::today(),
            assignment_label, 
            activity_id = activity_id,
        )

    if(nrow(used_assignments_data > 0)) {
        googlesheets4::sheet_append(used_assignments_sheet, assigned_applicants, sheet = 1)
    } else {
        googlesheets4::sheet_write(assigned_applicants, used_assignments_sheet, sheet = 1)
    }

    return(assigned_applicants)

}
