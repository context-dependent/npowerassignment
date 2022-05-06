#' Upload assignment list bundle to Google Drive
#' 
#' Function to create a new assignment list workbook 
#' in google sheets and log its creation in the activity log
#' 
#' @export
upload_assignment_list_bundle <- function(
    assignment_list_bundle,
    custom_tag = NULL,
    test = FALSE
) {

    if(!is.null(custom_tag)) {
        name_tag <- paste0("_", custom_tag)
    } else {
        name_tag <- ""
    }

    activity_id <- log_activity("Upload Assignment List Bundle", test = test) # nolint

    workbook_name <-
        glue::glue("assignment-list_{Sys.time()}{name_tag}")

    gs_workbook <-

        googledrive::drive_create(
            workbook_name,
            path = "assignment-lists",
            type = "spreadsheet"
        )

    assignment_list_bundle  |>
        purrr::iwalk(~googlesheets4::sheet_write(.x, gs_workbook, sheet = .y))

    googlesheets4::sheet_delete(gs_workbook, "Sheet1")

    return(gs_workbook)
}

#' Generate a bundle of assignment lists 
#' 
#' Create assignment lists with a range of treatment 
#' probabilities, whose incrememts are determined by the 
#' block size. 
#' 
#' @export
generate_assignment_list_bundle <- function(
    parameters = list()
) {

    activity_id <- log_activity("Generate Assignment List Bundle")

    treatment_probabilities <-

        seq(
            parameters$minimum_treatment_probability,
            parameters$maximum_treatment_probability,
            1 / parameters$block_size
        )

    assignment_lists <- treatment_probabilities  |>
        purrr::map(
            ~generate_assignment_list(parameters$minimum_list_size, .x, parameters$block_size)
        )  |>
        purrr::set_names(glue::glue(
            "block-{parameters$block_size}_p-trt-{treatment_probabilities * 100}"
        ))

    bundle_meta <- tibble::tibble(
        assignment_list_bundle_id = uuid::UUIDgenerate(1),
        activity_id = activity_id,
        list_length = nrow(assignment_lists[[1]]),
        minimum_treatment_probability = parameters$minimum_treatment_probability, 
        maximum_treatment_probability = parameters$maximum_treatment_probability, 
        treatment_probability_step_size = 1 / parameters$block_size,
        upload_timestamp = Sys.time()
    )

    assignment_list_bundle <- c(
        list(meta = bundle_meta), 
        assignment_lists
    )

    return(assignment_list_bundle)
}

#' Generate blank binary assignment list
#'
#' This uses block randomization to generate a blank list of assignments
#' of at least a given size with a treatment probability as close as possible
#' to the target
#' 
#' @export
generate_assignment_list <- function(
    minimum_list_size,
    target_treatment_probability,
    block_size
) {

    n_blocks <- minimum_list_size %/% block_size + 1

    assignment_list <- seq_len(n_blocks)  |>

        purrr::map(
            ~generate_assignment_block(target_treatment_probability, block_size)
        )  |>

        dplyr::bind_rows()  |>

        dplyr::mutate(
            assignment_id = uuid::UUIDgenerate(n = dplyr::n()),
            assignment_order_list = dplyr::row_number()
        )

    return(assignment_list)
}

#' Generate block for assignment list
#' 
#' Make a block of assignments of a given size
#' with a given treatment probability
#' 
#' @export
generate_assignment_block <- function(
    target_treatment_probability,
    block_size
) {
    n_treatment <- round(target_treatment_probability * block_size)
    n_control <- block_size - n_treatment
    base_assignments <- c(rep(1, n_treatment), rep(0, n_control))
    block <-

        tibble::tibble(
            param_treatment_probability =  n_treatment / block_size,
            param_block_size = block_size,
            assignment_treat = base_assignments,
            block_randomizer = runif(block_size)
        )  |>

        dplyr::arrange(block_randomizer)  |>

        dplyr::mutate(
            assignment_order_block = dplyr::row_number(),
            assignment_label = c("Control", "Treatment")[assignment_treat + 1]
        )

    return(block)

}
