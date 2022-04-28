#' Sythetic cohort of 500 NPower applicants from various programs
#' 
#' A dataset with synthetic, randomly generated applicants whose
#' characteristics are chosen to roughly represent the applicants
#' we will be randomly assigning
#' 
#' @format A tibble with 500 rows and 5 variables: 
#' \describe{
#'     \item{applicant_id}{unique identifier for each applicant}
#'     \item{gender}{applicant gender, from ['Man', 'Woman', 'Other']}
#'     \item{assignment_eligible}{1 if the applicant should be assigned}
#'     \item{location}{delivery location}
#'     \item{program}{program applied to}
#' }
"synthetic_cohort"

#' Default experiment parameters 
#' 
#' A list of default parameters to be used in stratification and randomization
#' 
#' @format a list with some number of named values
"default_parameters"