generate_synthetic_cohort <- function(n) {

    dat <- tibble::tibble(
        applicant_id = uuid::UUIDgenerate(n = n),
        application_timestamp = lubridate::as_datetime(
            sample(1632416029:1637686333, n)
        ),
        priority_gender_group = rbinom(n, 1, 0.4), 
        assignment_eligible = rbinom(n, 1, 0.8),
        location_program = sample(
            c(
                "Toronto-JITA", 
                "Toronto-JDA", 
                "Peel-JITA", 
                "Peel-JDA", 
                "York-JITA", 
                "Calgary-JITA", 
                "MLSE-JSQA"
            ), 
            n, replace = TRUE, 
            prob = c(
                50,
                50, 
                50, 
                50, 
                60, 
                50, 
                80
            ) / 390
        )
    ) %>%

    tidyr::separate(location_program, into = c("location", "program"))

    return(dat)
}

set.seed(489)

synthetic_cohort <- generate_synthetic_cohort(500)

usethis::use_data(synthetic_cohort, overwrite = TRUE)

