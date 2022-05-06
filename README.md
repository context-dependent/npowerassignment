
<!-- README.md is generated from README.Rmd. Please edit that file -->

# npowerassignment

<!-- badges: start -->
<!-- badges: end -->

The goal of npowerassignment is to manage random assignment for the
NPower RCT.

It manages the initialization of the experiment, assigning cohorts of
participants, and logging their assignments.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("context-dependent/npowerassignment")
```

## Example

To initialize the experiment:

``` r
library(npowerassignment)
## basic example code

initialize_experiment(
  custom_parameters = list(
    minimum_list_size = 1000, 
    overall_treatment_probability = 0.5,
    minimum_treatment_probability = 0.2, 
    maximum_treatment_probability = 0.8, 
    eligible_locations = "Greater Toronto Area - GTA, ON", 
    eligible_programs = "Junior IT Analyst Program (JITA)",
    rep_target_gender_diversity = 0.5
  )
)
```

Assign a cohort of applicants

``` r
applicants <- sample_cohort

assign_cohort(applicants)
```
