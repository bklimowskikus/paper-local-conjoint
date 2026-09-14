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

dominance_bootstrap_a <- collect_dominance(
  prepared_candidate_data,
  bootstrap_replicates = 19L,
  seed = 20240801L
)
dominance_bootstrap_b <- collect_dominance(
  prepared_candidate_data,
  bootstrap_replicates = 19L,
  seed = 20240801L
)
dominance_comparison_a <- attr(
  dominance_bootstrap_a,
  "roots_government_comparison"
)
dominance_comparison_b <- attr(
  dominance_bootstrap_b,
  "roots_government_comparison"
)
stopifnot(
  isTRUE(all.equal(
    centered_bootstrap_p(2, c(1, 2, 3, 4)),
    0.4
  )),
  identical(
    dominance_bootstrap_a[c("conf.low", "conf.high")],
    dominance_bootstrap_b[c("conf.low", "conf.high")]
  ),
  identical(dominance_comparison_a, dominance_comparison_b),
  nrow(dominance_comparison_a) == 1L,
  identical(
    dominance_comparison_a$contrast,
    "Municipal roots minus government-opposition cue"
  ),
  dominance_comparison_a$estimate > 0,
  dominance_comparison_a$p.value >= 0,
  dominance_comparison_a$p.value <= 1,
  identical(unique(dominance_bootstrap_a$bootstrap_unit), "respondent"),
  identical(unique(dominance_bootstrap_a$bootstrap_replicates), 19L),
  all(dominance_bootstrap_a$conf.low >= 0),
  all(dominance_bootstrap_a$conf.high <= 1),
  all(dominance_bootstrap_a$conf.low <= dominance_bootstrap_a$conf.high)
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
  "results/paper_dominance.csv",
  "results/paper_dominance_comparison.csv",
  "results/paper_municip_by_office.csv",
  "results/paper_office_interaction_test.csv",
  "results/paper_office_pairwise.csv",
  "results/paper_alignment_marginal_means.csv",
  "results/paper_alignment_group_distribution.csv",
  "results/paper_alignment_differences.csv",
  "results/paper_alignment_sensitivity.csv",
  "results/paper_alignment_interaction_tests.csv",
  "results/paper_alignment_cross_profile.csv",
  "results/paper_robustness.csv",
  "results/paper_balance.csv",
  "results/paper_respondent_balance.csv",
  "results/paper_advantage_distribution.csv",
  "results/paper_full_model_estimates.csv",
  "results/figures/paper_main_effects.png",
  "results/figures/paper_dominance.png",
  "results/figures/paper_municip_by_office.png",
  "results/figures/paper_cue_heterogeneity.png"
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
alignment_groups <- read.csv(
  "results/paper_alignment_group_distribution.csv",
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
dominance <- read.csv(
  "results/paper_dominance.csv",
  stringsAsFactors = FALSE
)
dominance_comparison <- read.csv(
  "results/paper_dominance_comparison.csv",
  stringsAsFactors = FALSE
)
figure_files <- required_files[grepl("results/figures/", required_files)]

has_vertical_reference <- function(plot) {
  any(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomVline"),
    logical(1)
  ))
}

has_cross_profile_bracket <- function(plot) {
  segment_layers <- lapply(ggplot_build(plot)$data, function(layer) {
    required <- c("x", "xend", "y", "yend")
    if (!all(required %in% names(layer))) {
      return(NULL)
    }
    layer[, required]
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

uses_only_achromatic_colors <- function(plot) {
  colors <- unique(unlist(lapply(ggplot_build(plot)$data, function(layer) {
    unlist(layer[intersect(c("colour", "fill"), names(layer))], use.names = FALSE)
  })))
  colors <- as.character(colors[!is.na(colors)])
  if (!length(colors)) {
    return(TRUE)
  }
  rgb <- grDevices::col2rgb(colors)
  all(rgb[1, ] == rgb[2, ] & rgb[2, ] == rgb[3, ])
}

has_dominance_bracket <- function(plot) {
  segment_count <- sum(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomSegment"),
    logical(1)
  ))
  labels <- unlist(lapply(ggplot_build(plot)$data, function(layer) {
    if ("label" %in% names(layer)) as.character(layer$label) else character()
  }))
  segment_count == 3L && any(grepl("^p = ", labels))
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
dominance_plot <- make_dominance_plot(dominance, dominance_comparison)
dominance_plot_layers <- ggplot_build(dominance_plot)$data
dominance_bracket_clearance <-
  dominance_plot_layers[[3]]$y[1] - max(dominance_plot_layers[[2]]$y)
interaction_top <- interaction_plot[[1]]
interaction_bottom <- interaction_plot[[2]]
interaction_bottom_plots <- attr(interaction_bottom, "grobs")$full
all_interaction_shapes <- c(
  unname(shape_values),
  unname(municip_shape_values)
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
  setequal(
    alignment_groups$respondent_gov,
    c("anti_respondent", "mixed_respondent", "pro_respondent")
  ),
  sum(alignment_groups$n) == 2072L,
  isTRUE(all.equal(sum(alignment_groups$percent), 100)),
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
  nrow(dominance) == 6L,
  setequal(
    dominance$attribute,
    c("age", "sex", "occupation", "government", "municip", "key_issue")
  ),
  identical(unique(dominance$model), "respondent_fixed_effects"),
  all(is.finite(dominance$within_r_squared)),
  identical(dominance$rank, seq_len(nrow(dominance))),
  all(diff(dominance$relative_importance) <= 0),
  all(dominance$general_dominance >= 0),
  identical(unique(dominance$bootstrap_unit), "respondent"),
  identical(unique(dominance$bootstrap_replicates), 4999L),
  all(dominance$conf.low >= 0),
  all(dominance$conf.high <= 1),
  all(dominance$conf.low <= dominance$relative_importance),
  all(dominance$relative_importance <= dominance$conf.high),
  nrow(dominance_comparison) == 1L,
  dominance_comparison$estimate > 0,
  dominance_comparison$conf.low <= dominance_comparison$estimate,
  dominance_comparison$estimate <= dominance_comparison$conf.high,
  dominance_comparison$p.value >= 0,
  dominance_comparison$p.value <= 1,
  isTRUE(all.equal(
    sum(dominance$relative_importance),
    1,
    tolerance = 1e-10
  )),
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
  inherits(dominance_plot$coordinates, "CoordFlip"),
  any(vapply(
    dominance_plot$layers,
    function(layer) inherits(layer$geom, "GeomPointrange"),
    logical(1)
  )),
  has_dominance_bracket(dominance_plot),
  dominance_bracket_clearance >= 0.045,
  !inherits(interaction_top, "patchwork"),
  inherits(interaction_bottom, "wrapped_patch"),
  identical(interaction_top$labels$title, "Marginal means"),
  identical(
    interaction_bottom_plots$patches$annotation$title,
    "Differences in marginal means"
  ),
  length(unique(all_interaction_shapes)) == length(all_interaction_shapes),
  has_vertical_reference(interaction_top),
  has_vertical_reference(interaction_bottom_plots[[1]]),
  has_vertical_reference(interaction_bottom_plots[[2]]),
  has_cross_profile_bracket(interaction_top),
  uses_only_achromatic_colors(main_plot[[1]]),
  uses_only_achromatic_colors(main_plot[[2]]),
  uses_only_achromatic_colors(dominance_plot),
  uses_only_achromatic_colors(office_plot),
  uses_only_achromatic_colors(interaction_top),
  uses_only_achromatic_colors(interaction_bottom_plots[[1]]),
  uses_only_achromatic_colors(interaction_bottom_plots[[2]]),
  all(file.info(figure_files)$size > 0),
  !dir.exists("paper")
)

message("Reproduction package checks passed")
