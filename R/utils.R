log_activity <- function(activity_label, first = FALSE, test = FALSE) {
    activity_id <- uuid::UUIDgenerate(1)
    activity_record <- tibble::tibble(
        actibity_id = activity_id,
        activity_type = activity_label,
        activity_time = Sys.time(), 
        test_flag = test
    )

    activity_log <- googledrive::drive_find("activity-log")  |>
        googlesheets4::gs4_get()

    if(first) {
        googlesheets4::sheet_write(activity_record, activity_log,  sheet = "Sheet1")
    } else {
        googlesheets4::sheet_append(activity_log, activity_record)
    }

    return(activity_id)
}

get_latest_assignment_bundle <- function() {
    bundles <- googledrive::drive_ls(path = "assignment-lists")
    latest_bundle <- bundles  |>
        dplyr::arrange(dplyr::desc(name))  |>
        dplyr::slice(1)  |>
        googlesheets4::gs4_get()

    return(latest_bundle)
}

get_latest_stratification_parameters <- function() {

    stratification_parameters <- googledrive::drive_find("stratification-parameters")  |>
        googlesheets4::gs4_get()  |>
        googlesheets4::read_sheet()  |>
        dplyr::arrange(dplyr::desc(date_time_created))  |>
        dplyr::slice(1)  |>
        as.list()

    return(stratification_parameters)

}

get_used_assignments <- function() {
    used_assignments <- googledrive::drive_ls(path = "assignments") |>
        googlesheets4::gs4_get() 

    return(used_assignments)
    
}

#' Modify active stratification parameters
#' 
#' Replace specific stratification parameters with 
update_stratification_parameters <- function(
    parameter_updates = list(),
    test = FALSE
) {
    activity_id <- log_activity("Update Stratification Parameters")
    latest_parameters <- get_latest_stratification_parameters()
    updated_parameters <- latest_parameters  |> modifyList(parameter_updates)  |>
        tibble::as_tibble()  |>
        dplyr::mutate(
            date_time_created = Sys.time(), 
            activity_id = activity_id
        )

    wb_stratification_parameters <- googledrive::drive_find("stratification-parameters")  |>
        googlesheets4::gs4_get()

    wb_stratification_parameters  |>
        googlesheets4::sheet_append(updated_parameters)

    invisible(wb_stratification_parameters)
}