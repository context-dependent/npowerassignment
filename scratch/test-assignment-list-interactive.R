devtools::load_all()

initialize_experiment(
    custom_parameters = list(
        minimum_list_size = 1000
    )
)

applicant_data <- read_applicant_file("data-raw/sample-report.csv")
assign_cohort(applicant_data)

