library(targets)

tar_option_set(
  packages = c(
    "broom",
    "dplyr",
    "dominanceanalysis",
    "ggplot2",
    "haven",
    "marginaleffects",
    "patchwork",
    "purrr",
    "readr",
    "scales",
    "stringr",
    "survey",
    "tibble",
    "tidyr"
  ),
  seed = 1234,
  format = "rds"
)

tar_source("R")

list(
  tar_target(
    conjoint_data,
    "data/conjoint_1fala.dta",
    format = "file"
  ),
  tar_target(
    respondent_data,
    "data/PLSW_1-2fala_final.dta",
    format = "file"
  ),
  tar_target(
    analysis_outputs,
    build_paper_outputs(
      conjoint_data,
      respondent_data,
      figures_dir = "results/figures"
    ),
    format = "file"
  )
)
