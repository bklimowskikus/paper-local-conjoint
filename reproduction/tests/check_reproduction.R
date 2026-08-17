#!/usr/bin/env Rscript

metadata_files <- c(
  "README.md",
  "data/README.md",
  ".gitignore",
  ".Rprofile",
  ".Renviron",
  "renv.lock",
  "renv/activate.R"
)

missing_metadata <- metadata_files[!file.exists(metadata_files)]
if (length(missing_metadata)) {
  stop(
    "Missing reproduction metadata: ",
    paste(missing_metadata, collapse = ", "),
    call. = FALSE
  )
}

lockfile_lines <- readLines("renv.lock", warn = FALSE)
if (any(grepl('"Source": "unknown"', lockfile_lines, fixed = TRUE))) {
  stop("renv.lock contains an unknown package source", call. = FALSE)
}

rcpp_imports <- packageDescription("RcppArmadillo")$Imports
rcpp_match <- regmatches(
  rcpp_imports,
  regexec("Rcpp \\(>= ([^)]+)\\)", rcpp_imports)
)[[1]]
if (
  length(rcpp_match) == 2L &&
    packageVersion("Rcpp") < package_version(rcpp_match[[2]])
) {
  stop("Rcpp does not satisfy the RcppArmadillo requirement", call. = FALSE)
}

source("R/analysis.R")

alignment_fixture <- tibble::tibble(
  ID = factor(1:6),
  government = factor(
    c(
      "pro_government", "anti_government", "neutral",
      "anti_government", "pro_government", "pro_government"
    ),
    levels = c("neutral", "pro_government", "anti_government")
  )
)
respondent_fixture <- tibble::tibble(
  ID = factor(1:6),
  respondent_gov = factor(
    c(
      "pro_respondent", "pro_respondent", "pro_respondent",
      "anti_respondent", "anti_respondent", "mixed_respondent"
    ),
    levels = c("anti_respondent", "mixed_respondent", "pro_respondent")
  )
)
alignment_result <- add_political_alignment(
  alignment_fixture,
  respondent_fixture
)
stopifnot(
  nrow(alignment_result) == 4L,
  setequal(
    as.character(alignment_result$political_alignment),
    c("non_aligned", "aligned")
  ),
  identical(
    levels(alignment_result$political_alignment),
    c("non_aligned", "aligned")
  )
)

required_files <- c(
  "results/paper_design_summary.csv",
  "results/paper_main_effects.csv",
  "results/paper_municip_by_office.csv",
  "results/paper_office_interaction_test.csv",
  "results/paper_alignment_marginal_means.csv",
  "results/paper_alignment_differences.csv",
  "results/paper_alignment_sensitivity.csv",
  "results/paper_robustness.csv",
  "results/paper_balance.csv",
  "results/paper_advantage_distribution.csv",
  "results/paper_full_model_estimates.csv",
  "../paper_draft/index.qmd",
  "../paper_draft/_quarto.yml",
  "../paper_draft/paper.scss",
  "../paper_draft/references.bib",
  "../paper_draft/figs/paper_main_effects.png",
  "../paper_draft/figs/paper_municip_by_office.png",
  "../paper_draft/figs/paper_cue_heterogeneity.png"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Missing reproduction artifacts: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

design <- read.csv(
  "results/paper_design_summary.csv",
  stringsAsFactors = FALSE
)
alignment <- read.csv(
  "results/paper_alignment_marginal_means.csv",
  stringsAsFactors = FALSE
)
office_test <- read.csv(
  "results/paper_office_interaction_test.csv",
  stringsAsFactors = FALSE
)
sensitivity <- read.csv(
  "results/paper_alignment_sensitivity.csv",
  stringsAsFactors = FALSE
)
robustness <- read.csv(
  "results/paper_robustness.csv",
  stringsAsFactors = FALSE
)
manuscript_lines <- readLines("../paper_draft/index.qmd", warn = FALSE)
figure_files <- required_files[grepl("paper_draft/figs/", required_files)]

has_vertical_reference <- function(plot) {
  any(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomVline"),
    logical(1)
  ))
}

main_effects <- read.csv(
  "results/paper_main_effects.csv",
  stringsAsFactors = FALSE
)
office_mms <- read.csv(
  "results/paper_municip_by_office.csv",
  stringsAsFactors = FALSE
)
alignment_differences <- read.csv(
  "results/paper_alignment_differences.csv",
  stringsAsFactors = FALSE
)
main_plot <- make_main_effect_plot(main_effects)
office_plot <- make_office_plot(office_mms)
interaction_plot <- make_interaction_plot(alignment, alignment_differences)

stopifnot(
  sum(design$respondents) == 2207L,
  sum(design$profiles) == 17656L,
  setequal(
    unique(alignment$political_alignment),
    c("non_aligned", "aligned")
  ),
  nrow(office_test) == 1L,
  identical(office_test$term, "municip:office"),
  is.finite(office_test$statistic),
  is.finite(office_test$p.value),
  office_test$p.value >= 0,
  office_test$p.value <= 1,
  nrow(sensitivity) == 9L,
  setequal(unique(sensitivity$coding), c("narrow", "primary", "broad")),
  all(is.finite(sensitivity$estimate)),
  setequal(
    unique(robustness$weight),
    c("unweighted", "census_margins", "census_vote_2023")
  ),
  has_vertical_reference(main_plot$patches$plots[[1]]),
  has_vertical_reference(office_plot),
  has_vertical_reference(
    interaction_plot$patches$plots[[1]]$patches$plots[[1]]
  ),
  has_vertical_reference(
    interaction_plot$patches$plots[[2]]$patches$plots[[1]]
  ),
  !any(grepl("waga1|waga2", manuscript_lines)),
  any(grepl("2,207 respondents", manuscript_lines, fixed = TRUE)),
  any(grepl("13.4 percentage points", manuscript_lines, fixed = TRUE)),
  all(file.info(figure_files)$size > 0),
  !dir.exists("paper")
)

message("Reproduction package checks passed")
