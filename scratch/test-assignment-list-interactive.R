devtools::load_all()

initialize_experiment()

applicant_data <- read_applicant_file("data-raw/sample-report.csv")
assign_cohort(applicant_data)

