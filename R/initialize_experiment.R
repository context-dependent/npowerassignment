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
    log_activity("Initialize Experiment")
    initial_parameters <- modifyList(default_parameters, custom_parameters)
    set.seed(initial_parameters$seed)
    update_stratification_parameters(initial_parameters, first = TRUE, test = test)
    message("Experiment successfully initialized")

    invisible()

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
        ) |>
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
