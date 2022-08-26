library(tidyverse)

synthetic_cohort |>
    summarize(mean_gender = mean(priority_gender_group))

synthetic_cohort |>
    mutate(
        trt_naive = rbinom(n(), 1, 0.5), 
        trt_complement = case_when(
            priority_gender_group == 1 ~ rbinom(n(), 1, 0.6), 
            TRUE ~ rbinom(n(), 1, 0.4)
        ), 
        trt_overcorrect = case_when(
            priority_gender_group == 1 ~ rbinom(n(), 1, 0.90), 
            TRUE ~ rbinom(n(), 1, 0.10)
        ), 
    ) |>

    group_by(trt_overcorrect) |>
    summarize(
        mean_gender = mean(priority_gender_group)
    )




