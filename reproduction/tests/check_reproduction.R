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

raw_candidate_data <- haven::read_dta("data/conjoint_1fala.dta")
prepared_candidate_data <- prepare_conjoint_data("data/conjoint_1fala.dta")
stopifnot(
  nrow(prepared_candidate_data) == nrow(raw_candidate_data),
  all(prepared_candidate_data$sex[raw_candidate_data$sex == 1] == "female"),
  all(prepared_candidate_data$sex[raw_candidate_data$sex == 2] == "male")
)

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
  "results/paper_office_pairwise.csv",
  "results/paper_alignment_marginal_means.csv",
  "results/paper_alignment_differences.csv",
  "results/paper_alignment_sensitivity.csv",
  "results/paper_alignment_interaction_tests.csv",
  "results/paper_alignment_cross_profile.csv",
  "results/paper_robustness.csv",
  "results/paper_balance.csv",
  "results/paper_respondent_balance.csv",
  "results/paper_advantage_distribution.csv",
  "results/paper_full_model_estimates.csv",
  "../paper_draft/v2/index.qmd",
  "../paper_draft/v2/_quarto.yml",
  "../paper_draft/v2/paper.scss",
  "../paper_draft/v2/references.bib",
  "../paper_draft/v2/figs/paper_main_effects.png",
  "../paper_draft/v2/figs/paper_municip_by_office.png",
  "../paper_draft/v2/figs/paper_cue_heterogeneity.png"
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
alignment_differences <- read.csv(
  "results/paper_alignment_differences.csv",
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
office_pairwise <- read.csv(
  "results/paper_office_pairwise.csv",
  stringsAsFactors = FALSE
)
alignment_interactions <- read.csv(
  "results/paper_alignment_interaction_tests.csv",
  stringsAsFactors = FALSE
)
alignment_cross_profile <- read.csv(
  "results/paper_alignment_cross_profile.csv",
  stringsAsFactors = FALSE
)
respondent_balance <- read.csv(
  "results/paper_respondent_balance.csv",
  stringsAsFactors = FALSE
)
robustness <- read.csv(
  "results/paper_robustness.csv",
  stringsAsFactors = FALSE
)
manuscript_lines <- readLines("../paper_draft/v2/index.qmd", warn = FALSE)
figure_files <- required_files[grepl("paper_draft/v2/figs/", required_files)]

has_vertical_reference <- function(plot) {
  any(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomVline"),
    logical(1)
  ))
}

has_cross_profile_bracket <- function(plot) {
  segment_layers <- lapply(ggplot_build(plot)$data, function(layer) {
    required <- c("x", "xend", "y", "yend", "colour")
    if (!all(required %in% names(layer))) {
      return(NULL)
    }
    layer[layer$colour == "#D55E00", required]
  })
  segment_layers <- Filter(Negate(is.null), segment_layers)
  if (!length(segment_layers)) {
    return(FALSE)
  }

  segments <- do.call(rbind, segment_layers)
  horizontal <- with(
    segments,
    any(abs(y - yend) < 1e-10 & abs(x - xend) > 1e-10)
  )
  vertical <- with(
    segments,
    sum(abs(x - xend) < 1e-10 & abs(y - yend) > 1e-10) >= 2L
  )

  horizontal && vertical
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
interaction_plot <- make_interaction_plot(
  alignment,
  alignment_differences,
  alignment_cross_profile
)
plotted_cross_profile_difference <- with(
  alignment,
  estimate[political_alignment == "aligned" & municip == "outside_knows_issues"] -
    estimate[political_alignment == "non_aligned" & municip == "local_since_birth"]
)
reported_cross_profile_difference <- alignment_cross_profile$estimate[
  alignment_cross_profile$contrast ==
    "Aligned outsider minus non-aligned lifelong resident"
]

stopifnot(
  sum(design$respondents) == 2207L,
  sum(design$profiles) == 17656L,
  setequal(
    unique(alignment$political_alignment),
    c("non_aligned", "aligned")
  ),
  nrow(alignment_differences) == 7L,
  "p_holm" %in% names(alignment_differences),
  all(alignment_differences$p_holm >= alignment_differences$p.value),
  nrow(office_test) == 1L,
  identical(office_test$term, "municip:office"),
  is.finite(office_test$statistic),
  is.finite(office_test$p.value),
  office_test$p.value >= 0,
  office_test$p.value <= 1,
  nrow(office_pairwise) == 9L,
  all(office_pairwise$p_holm >= office_pairwise$p.value),
  nrow(sensitivity) == 9L,
  setequal(unique(sensitivity$coding), c("narrow", "primary", "broad")),
  all(is.finite(sensitivity$estimate)),
  setequal(
    unique(robustness$weight),
    c("unweighted", "census_margins", "census_vote_2023")
  ),
  nrow(alignment_interactions) == 12L,
  nrow(alignment_cross_profile) == 2L,
  isTRUE(all.equal(
    plotted_cross_profile_difference,
    reported_cross_profile_difference,
    tolerance = 1e-10
  )),
  nrow(respondent_balance) == 5L,
  all(respondent_balance$cramers_v >= 0),
  has_vertical_reference(main_plot$patches$plots[[1]]),
  has_vertical_reference(office_plot),
  has_vertical_reference(
    interaction_plot$patches$plots[[1]]$patches$plots[[1]]
  ),
  has_vertical_reference(
    interaction_plot$patches$plots[[2]]$patches$plots[[1]]
  ),
  has_cross_profile_bracket(
    interaction_plot$patches$plots[[1]]$patches$plots[[1]]
  ),
  has_cross_profile_bracket(
    interaction_plot$patches$plots[[1]]
  ),
  !any(grepl("second-largest", manuscript_lines, fixed = TRUE)),
  !any(grepl("residence adds", manuscript_lines, fixed = TRUE)),
  !any(grepl("residence gains", manuscript_lines, fixed = TRUE)),
  sum(grepl(
    "lifelong-resident statement increases selection by 18.6 points relative to the outsider-with-local-knowledge statement",
    manuscript_lines,
    fixed = TRUE
  )) >= 4L,
  any(grepl("#tbl-figure3-pairwise", manuscript_lines, fixed = TRUE)),
  !any(grepl("waga1|waga2", manuscript_lines)),
  any(grepl("2,207 respondents", manuscript_lines, fixed = TRUE)),
  any(grepl("13.4 percentage points", manuscript_lines, fixed = TRUE)),
  all(file.info(figure_files)$size > 0),
  !dir.exists("paper")
)

message("Reproduction package checks passed")
