log_activity <- function(activity_label, first = FALSE, test = FALSE) {
    activity_id <- uuid::UUIDgenerate(1)
    activity_record <- tibble::tibble(
        actibity_id = activity_id,
        activity_type = activity_label,
        activity_time = Sys.time(), 
        test_flag = test
    )

    activity_record |> write_or_append("activity-log")

    return(activity_id)
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
    dr_used_assignments <- googledrive::drive_find("assignments") 
    
    if(nrow(dr_used_assignments) == 0) {
        wb_used_assignments <- NULL  
    } else {
        wb_used_assignments <- dr_used_assignments |>
            googlesheets4::gs4_get() 
    }

    return(wb_used_assignments)
    
}

#' Modify active stratification parameters
#' 
#' Replace specific stratification parameters with 
update_stratification_parameters <- function(
    parameter_updates = list(),
    first = FALSE, 
    test = FALSE
) {
    activity_id <- log_activity("Update Stratification Parameters")
    if(!first) {
        latest_parameters <- get_latest_stratification_parameters()
        updated_parameters <- latest_parameters  |> 
            modifyList(parameter_updates) 
    } else {
        updated_parameters <- parameter_updates
    }
    
    updated_parameters |>
        tibble::as_tibble()  |>
        dplyr::mutate(
            date_time_created = Sys.time(), 
            activity_id = activity_id
        ) |>
        write_or_append("stratification-parameters")

    invisible()
}

write_or_append <- function(data, wb_name, sheet = "Sheet1") {
    
    dr <- googledrive::drive_find(wb_name, type = "spreadsheet")

    if(nrow(dr) > 0) {
        wb <- googlesheets4::gs4_get(dr)
        df <- wb |> googlesheets4::read_sheet(sheet = sheet)

        if(nrow(df) > 0) {
            googlesheets4::sheet_append(wb, data, sheet = sheet)
        } 
    } else {
        
        sheets_in <- list(x = data)
        names(sheets_in) <- sheet
        wb <- googlesheets4::gs4_create(wb_name, sheets = sheets_in)
    }

    googlesheets4::sheet_write(data, wb, sheet = sheet)

    invisible()

}
