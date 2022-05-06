#' Initialize the NPower RCT
#' 
#' Creates three googlesheets in the npowerassginmentdatalink 
#' google drive: 
#'   - activity-log: tracks activities, including assignment list creation,
#'     assignment batch execution, and stratification parameter updates. 
#'   - stratification-parameters: records initial experiment parameters and 
#'     parameter updates.
#'   - assignment-list/assignment-list_[timestamp]
#' @param custom_parameters
#' A list of parameters to update, from among the following: 
#'   - minimum_list_size = 5000, 
#'   - minimum_treatment_probability = 0.2, 
#'   - maximum_treatment_probability = 0.7, 
#'   - block_size = 20,
#'   - rep_target_gender_diversity = 0.5, 
#'   - eligible_programs = "JITA", 
#'   - eligible_locations = "Toronto;Peel;York"
#' @export 

initialize_experiment <- function(
    custom_parameters = list(),
    test = FALSE
) {
    initial_parameters <- modifyList(default_parameters, custom_parameters)
    create_folder_structure()
    create_activity_log(test = test)
    create_stratification_parameters(initial_parameters,  test = test)
    assignment_list_bundle <- generate_assignment_list_bundle(
        initial_parameters
    )
    assignment_list_wb <- upload_assignment_list_bundle(
        assignment_list_bundle,
        test = test
    )
    create_assignments_table()

    message("Experiment successfully initialized")

    invisible()

}

create_folder_structure <- function(
    folder_names = c("assignment-lists", "assignments")
) {
    folder_names |>
        purrr::walk(
            function(x) {
                googledrive::drive_mkdir(x, path = "", overwrite = TRUE)
            }
        )

    invisible()
}

create_activity_log <- function(test = FALSE) {

    wb_activity_log <- googledrive::drive_create("activity-log",  type = "spreadsheet", overwrite = TRUE)
    log_activity("Initialize Experiment", first = TRUE, test = test)
    log_activity("Create Activity Log", test = test)

    invisible(wb_activity_log)
}

create_stratification_parameters <- function( 
    initial_parameters = list(),
    test = FALSE, 
    browse = FALSE
) {
    if(browse) browser()
    activity_id <- log_activity("Create Stratification Parameters")

    wb_stratification_parameters <- 
        googledrive::drive_create(
            "stratification-parameters", 
            type = "spreadsheet",
            overwrite = TRUE
        ) %>%
        googlesheets4::gs4_get()
    
    googlesheets4::sheet_write(
        tibble::tibble(
            !!!initial_parameters, 
            date_time_created = Sys.time(), 
            activity_id = activity_id
        ), 
        wb_stratification_parameters,  
        sheet = 1
    )

    invisible(wb_stratification_parameters)


}

create_assignments_table <- function() {
    
    activity_id <- log_activity("Create Assignments Table")

    wb_assignments <- googledrive::drive_create(
        "assignments", 
        path = "assignments", 
        type = "spreadsheet", 
        overwrite = TRUE
    )

    assignments_schema <- tibble::tibble(
        assignment_list_bundle_id = character(), 
        assignment_list_bundle_name = character(),
        assignment_list_name = character(), 
        applicant_id = character(), 
        assignment_id = character(),
        assignment_date = character(), 
        assignment_label = character(), 
        activity_id = character(),
    )

    googlesheets4::sheet_write(
        assignments_schema, 
        wb_assignments, 
        sheet = 1
    )

    invisible()

}
