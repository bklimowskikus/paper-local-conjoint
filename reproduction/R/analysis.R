#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(broom)
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(marginaleffects)
  library(patchwork)
  library(readr)
  library(scales)
  library(stringr)
  library(survey)
  library(tibble)
  library(tidyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

theme_paper <- function() {
  theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0),
      strip.background = element_rect(fill = "grey92", color = NA),
      legend.position = "top",
      legend.justification = "left",
      plot.title = element_text(size = 10, face = "plain", hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 9)
    )
}

attribute_lookup <- tribble(
  ~attribute, ~attribute_nice, ~attribute_order,
  "age", "Candidate age", 1L,
  "sex", "Candidate sex", 2L,
  "occupation", "Occupation", 3L,
  "government", "Government-opposition cue", 4L,
  "municip", "Municipal roots", 5L,
  "key_issue", "Policy priority", 6L
)

level_lookup <- tribble(
  ~attribute, ~level, ~level_nice, ~level_order, ~is_reference,
  "age", "31", "31", 1L, TRUE,
  "age", "43", "43", 2L, FALSE,
  "age", "55", "55", 3L, FALSE,
  "age", "67", "67", 4L, FALSE,
  "sex", "male", "Male", 1L, TRUE,
  "sex", "female", "Female", 2L, FALSE,
  "occupation", "lawyer", "Lawyer", 1L, TRUE,
  "occupation", "engineer", "Engineer", 2L, FALSE,
  "occupation", "history_teacher", "History teacher", 3L, FALSE,
  "occupation", "entrepreneur", "Entrepreneur", 4L, FALSE,
  "government", "neutral", "Neutral", 1L, TRUE,
  "government", "pro_government", "Pro-government", 2L, FALSE,
  "government", "anti_government", "Anti-government", 3L, FALSE,
  "municip", "outside_knows_issues", "Lives elsewhere; knows local issues", 1L, TRUE,
  "municip", "local_few_years", "Lives locally for a few years", 2L, FALSE,
  "municip", "local_since_birth", "Lives locally since birth", 3L, FALSE,
  "key_issue", "support_business", "Support business", 1L, TRUE,
  "key_issue", "improve_roads", "Improve roads", 2L, FALSE,
  "key_issue", "support_families", "Support families", 3L, FALSE,
  "key_issue", "support_elderly", "Support older residents", 4L, FALSE,
  "key_issue", "environment", "Protect the environment", 5L, FALSE
)

prepare_conjoint_data <- function(path = "data/conjoint_1fala.dta") {
  raw <- haven::read_dta(path)

  tibble(
    ID = factor(raw$ID),
    profile = raw$profile,
    waga1 = as.numeric(raw$waga1),
    waga2 = as.numeric(raw$waga2),
    competence = as.integer(raw$competence),
    advantage = as.numeric(raw$advantage),
    age = factor(raw$age, levels = c(31, 43, 55, 67), labels = c("31", "43", "55", "67")),
    sex = factor(
      dplyr::recode(as.character(raw$sex), `1` = "male", `2` = "female"),
      levels = c("male", "female")
    ),
    occupation = factor(
      raw$occupation,
      levels = c(1, 2, 3, 4),
      labels = c("lawyer", "engineer", "history_teacher", "entrepreneur")
    ),
    government = factor(
      raw$government,
      levels = c(2, 1, 3),
      labels = c("neutral", "pro_government", "anti_government")
    ),
    municip = factor(
      raw$municip,
      levels = c(1, 2, 3),
      labels = c("outside_knows_issues", "local_few_years", "local_since_birth")
    ),
    key_issue = factor(
      raw$key_issue,
      levels = c(1, 2, 3, 4, 5),
      labels = c("support_business", "improve_roads", "support_families", "support_elderly", "environment")
    ),
    office = factor(
      raw$conjoint_split,
      levels = c(1, 3, 2),
      labels = c("mp", "mayor", "councillor")
    )
  ) %>%
    drop_na(
      competence, advantage, age, sex, occupation, government, municip,
      key_issue, office, waga1, waga2
    )
}

prepare_respondent_groups <- function(
  path = "data/PLSW_1-2fala_final.dta",
  anti_max = 3,
  pro_min = 7
) {
  stopifnot(anti_max < pro_min)

  haven::read_dta(path) %>%
    transmute(
      ID = factor(ID),
      nat_govopp = as.numeric(nat_govopp),
      respondent_gov = case_when(
        is.na(nat_govopp) ~ NA_character_,
        nat_govopp <= anti_max ~ "anti_respondent",
        nat_govopp >= pro_min ~ "pro_respondent",
        TRUE ~ "mixed_respondent"
      ),
      respondent_gov = factor(
        respondent_gov,
        levels = c("anti_respondent", "mixed_respondent", "pro_respondent")
      )
    ) %>%
    distinct(ID, .keep_all = TRUE)
}

add_political_alignment <- function(conjoint, respondent_groups) {
  conjoint %>%
    left_join(respondent_groups, by = "ID") %>%
    drop_na(respondent_gov) %>%
    filter(
      respondent_gov != "mixed_respondent",
      government != "neutral"
    ) %>%
    mutate(
      respondent_gov = droplevels(respondent_gov),
      government = droplevels(government),
      political_alignment = if_else(
        (respondent_gov == "pro_respondent" & government == "pro_government") |
          (respondent_gov == "anti_respondent" & government == "anti_government"),
        "aligned",
        "non_aligned"
      ),
      political_alignment = factor(
        political_alignment,
        levels = c("non_aligned", "aligned")
      )
    )
}

make_design <- function(data, weight = c("unweighted", "waga1", "waga2")) {
  weight <- match.arg(weight)
  weight_formula <- switch(
    weight,
    unweighted = ~1,
    waga1 = ~waga1,
    waga2 = ~waga2
  )
  svydesign(ids = ~ID, weights = weight_formula, data = data)
}

fit_lpm <- function(data, formula, weight = "unweighted") {
  svyglm(
    formula,
    design = make_design(data, weight),
    family = gaussian()
  )
}

estimand_weights <- function(fit) {
  as.numeric(weights(fit$survey.design))
}

estimand_df <- function(fit) {
  survey::degf(fit$survey.design)
}

estimate_mm <- function(fit, variable, by = NULL) {
  avg_predictions(
    fit,
    variables = variable,
    by = c(by, variable),
    wts = estimand_weights(fit),
    df = estimand_df(fit),
    type = "response"
  ) %>%
    as_tibble()
}

estimate_reference_difference <- function(fit, variable, by = NULL) {
  avg_comparisons(
    fit,
    variables = setNames(list("reference"), variable),
    by = by %||% TRUE,
    wts = estimand_weights(fit),
    df = estimand_df(fit),
    type = "response"
  ) %>%
    as_tibble()
}

collect_main_estimands <- function(fit) {
  variables <- attribute_lookup$attribute

  purrr::map_dfr(variables, function(variable) {
    reference_level <- level_lookup %>%
      filter(attribute == variable, is_reference) %>%
      pull(level)

    mm <- estimate_mm(fit, variable) %>%
      transmute(
        attribute = variable,
        level = as.character(.data[[variable]]),
        estimand = "MM",
        contrast = NA_character_,
        estimate,
        std.error,
        conf.low,
        conf.high,
        p.value
      )

    amce <- estimate_reference_difference(fit, variable) %>%
      transmute(
        attribute = variable,
        level = str_remove(contrast, " - .*"),
        estimand = "AMCE",
        contrast,
        estimate,
        std.error,
        conf.low,
        conf.high,
        p.value
      )

    reference <- tibble(
      attribute = variable,
      level = reference_level,
      estimand = "AMCE",
      contrast = "Reference",
      estimate = 0,
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    )

    bind_rows(mm, reference, amce)
  }) %>%
    left_join(attribute_lookup, by = "attribute") %>%
    left_join(level_lookup, by = c("attribute", "level")) %>%
    arrange(attribute_order, level_order, estimand)
}

nice_office <- function(x) {
  recode(as.character(x), mp = "MP", mayor = "Mayor", councillor = "Councillor")
}

nice_alignment <- function(x) {
  recode(
    as.character(x),
    non_aligned = "Non-aligned",
    aligned = "Aligned"
  )
}

nice_municip <- function(x) {
  recode(
    as.character(x),
    outside_knows_issues = "Lives elsewhere; knows local issues",
    local_few_years = "Lives locally for a few years",
    local_since_birth = "Lives locally since birth"
  )
}

make_main_effect_plot <- function(main_effects) {
  key_levels <- level_lookup %>%
    mutate(attribute_order = match(attribute, attribute_lookup$attribute)) %>%
    arrange(attribute_order, level_order) %>%
    transmute(key = paste(attribute, level, sep = "::")) %>%
    pull(key)
  label_map <- setNames(
    level_lookup$level_nice,
    paste(level_lookup$attribute, level_lookup$level, sep = "::")
  )

  plot_data <- main_effects %>%
    mutate(
      attribute_nice = factor(attribute_nice, levels = attribute_lookup$attribute_nice),
      plot_level = factor(
        paste(attribute, level, sep = "::"),
        levels = rev(key_levels)
      )
    )

  mm_plot <- plot_data %>%
    filter(estimand == "MM") %>%
    ggplot(aes(x = estimate, y = plot_level)) +
    geom_vline(xintercept = 0.5, linewidth = 0.35, color = "grey55") +
    geom_segment(
      aes(x = conf.low, xend = conf.high, yend = plot_level),
      linewidth = 0.35,
      color = "grey35"
    ) +
    geom_point(size = 1.8, shape = 16) +
    facet_grid(attribute_nice ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_discrete(labels = label_map) +
    labs(
      x = "Predicted probability of being judged more competent",
      y = NULL,
      title = "Marginal means"
    ) +
    theme_paper()

  amce_plot <- plot_data %>%
    filter(estimand == "AMCE") %>%
    ggplot(aes(x = estimate, y = plot_level)) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey55") +
    geom_segment(
      data = ~filter(.x, !is_reference),
      aes(x = conf.low, xend = conf.high, yend = plot_level),
      linewidth = 0.35,
      color = "grey35"
    ) +
    geom_point(aes(shape = is_reference), size = 1.8) +
    geom_text(
      data = ~filter(.x, is_reference),
      aes(label = "ref."),
      nudge_x = 0.012,
      hjust = 0,
      size = 2.4,
      color = "grey35"
    ) +
    facet_grid(attribute_nice ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_discrete(labels = label_map) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
    labs(
      x = "Difference from the reference level",
      y = NULL,
      title = "AMCEs"
    ) +
    theme_paper()

  mm_plot + amce_plot +
    plot_layout(widths = c(1.15, 1))
}

make_office_plot <- function(office_mms) {
  plot_data <- office_mms %>%
    mutate(
      office_nice = factor(nice_office(office), levels = c("MP", "Mayor", "Councillor")),
      municip_nice = factor(
        nice_municip(municip),
        levels = rev(c(
          "Lives elsewhere; knows local issues",
          "Lives locally for a few years",
          "Lives locally since birth"
        ))
      )
    )

  ggplot(
    plot_data,
    aes(x = estimate, y = municip_nice, color = office_nice, shape = office_nice)
  ) +
    geom_vline(xintercept = 0.5, linewidth = 0.35, color = "grey55") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.5),
      linewidth = 0.35
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    coord_cartesian(xlim = c(0.25, 0.72)) +
    scale_color_manual(values = c("MP" = "#0072B2", "Mayor" = "#E69F00", "Councillor" = "#009E73")) +
    scale_shape_manual(values = c("MP" = 16, "Mayor" = 17, "Councillor" = 15)) +
    labs(
      x = "Predicted probability of being judged more competent",
      y = NULL,
      color = NULL,
      shape = NULL
    ) +
    theme_paper()
}

shape_values <- c("Non-aligned" = 1, "Aligned" = 16)
municip_shape_values <- c(
  "Lives elsewhere; knows local issues" = 1,
  "Lives locally for a few years" = 4,
  "Lives locally since birth" = 16
)

make_interaction_plot <- function(alignment_mms, alignment_differences) {
  mm_data <- alignment_mms %>%
    mutate(
      alignment_nice = factor(
        nice_alignment(political_alignment),
        levels = c("Non-aligned", "Aligned")
      ),
      municip_nice = factor(
        nice_municip(municip),
        levels = c(
          "Lives elsewhere; knows local issues",
          "Lives locally for a few years",
          "Lives locally since birth"
        )
      )
    )

  home_differences <- alignment_differences %>%
    filter(direction == "home_within_alignment") %>%
    mutate(
      alignment_nice = factor(
        nice_alignment(political_alignment),
        levels = c("Non-aligned", "Aligned")
      ),
      contrast_nice = recode(
        contrast,
        "local_few_years - outside_knows_issues" = "Few years local vs outsider",
        "local_since_birth - outside_knows_issues" = "Local since birth vs outsider"
      ),
      contrast_nice = factor(
        contrast_nice,
        levels = rev(c("Few years local vs outsider", "Local since birth vs outsider"))
      )
    )

  political_differences <- alignment_differences %>%
    filter(direction == "alignment_within_home") %>%
    mutate(
      municip_nice = factor(
        nice_municip(municip),
        levels = c(
          "Lives elsewhere; knows local issues",
          "Lives locally for a few years",
          "Lives locally since birth"
        )
      ),
      contrast_nice = recode(
        contrast,
        "aligned - non_aligned" = "Aligned vs non-aligned"
      ),
      contrast_nice = factor(
        contrast_nice,
        levels = "Aligned vs non-aligned"
      )
    )

  home_mm_plot <- ggplot(
    mm_data,
    aes(x = estimate, y = municip_nice, shape = alignment_nice, group = alignment_nice)
  ) +
    geom_vline(xintercept = 0.5, linewidth = 0.35, color = "grey55") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.55),
      linewidth = 0.35
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    coord_cartesian(xlim = c(0.20, 0.72)) +
    scale_shape_manual(values = shape_values) +
    labs(
      x = "Predicted probability",
      y = NULL,
      shape = NULL,
      title = "Marginal means",
      tag = "A"
    ) +
    theme_paper()

  home_difference_plot <- ggplot(
    home_differences,
    aes(x = estimate, y = contrast_nice, shape = alignment_nice, group = alignment_nice)
  ) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey55") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.55),
      linewidth = 0.35
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_shape_manual(values = shape_values) +
    labs(
      x = "Home difference in marginal means",
      y = NULL,
      shape = NULL,
      title = "Differences in marginal means"
    ) +
    theme_paper() +
    theme(legend.position = "none")

  alignment_mm_plot <- ggplot(
    mm_data,
    aes(x = estimate, y = alignment_nice, shape = municip_nice, group = municip_nice)
  ) +
    geom_vline(xintercept = 0.5, linewidth = 0.35, color = "grey55") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.55),
      linewidth = 0.35
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    coord_cartesian(xlim = c(0.20, 0.72)) +
    scale_shape_manual(values = municip_shape_values) +
    labs(
      x = "Predicted probability",
      y = NULL,
      shape = NULL,
      title = "Marginal means",
      tag = "B"
    ) +
    theme_paper()

  alignment_difference_plot <- ggplot(
    political_differences,
    aes(x = estimate, y = contrast_nice, shape = municip_nice, group = municip_nice)
  ) +
    geom_vline(xintercept = 0, linewidth = 0.35, color = "grey55") +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.55),
      linewidth = 0.35
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_shape_manual(values = municip_shape_values) +
    labs(
      x = "Government-alignment difference in marginal means",
      y = NULL,
      shape = NULL,
      title = "Differences in marginal means"
    ) +
    theme_paper() +
    theme(legend.position = "none")

  (home_mm_plot | home_difference_plot) /
    (alignment_mm_plot | alignment_difference_plot) +
    plot_layout(widths = c(1.25, 1))
}

nice_model_term <- function(term) {
  case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "age43" ~ "Age: 43 vs 31",
    term == "age55" ~ "Age: 55 vs 31",
    term == "age67" ~ "Age: 67 vs 31",
    term == "sexfemale" ~ "Female vs male",
    term == "occupationengineer" ~ "Engineer vs lawyer",
    term == "occupationhistory_teacher" ~ "History teacher vs lawyer",
    term == "occupationentrepreneur" ~ "Entrepreneur vs lawyer",
    term == "governmentpro_government" ~ "Pro-government vs neutral",
    term == "governmentanti_government" ~ "Anti-government vs neutral",
    term == "municiplocal_few_years" ~ "Few years local vs outsider",
    term == "municiplocal_since_birth" ~ "Local since birth vs outsider",
    term == "key_issueimprove_roads" ~ "Improve roads vs support business",
    term == "key_issuesupport_families" ~ "Support families vs support business",
    term == "key_issuesupport_elderly" ~ "Support older residents vs support business",
    term == "key_issueenvironment" ~ "Environment vs support business",
    term == "officemayor" ~ "Mayor vs MP prompt",
    term == "officecouncillor" ~ "Councillor vs MP prompt",
    TRUE ~ term
  )
}

validate_results <- function(
  conjoint,
  alignment_data,
  main_effects,
  office_mms,
  office_interaction_test,
  alignment_mms,
  alignment_differences,
  alignment_sensitivity,
  robustness,
  balance,
  advantage_distribution
) {
  stopifnot(
    n_distinct(conjoint$ID) == 2207L,
    nrow(conjoint) == 17656L,
    n_distinct(alignment_data$ID) == 1635L,
    nrow(alignment_data) == 8712L,
    nrow(main_effects) == 42L,
    !anyDuplicated(main_effects[c("attribute", "level", "estimand")]),
    !anyDuplicated(office_mms[c("office", "municip")]),
    nrow(office_interaction_test) == 1L,
    identical(office_interaction_test$term, "municip:office"),
    !anyDuplicated(alignment_mms[c("political_alignment", "municip")]),
    setequal(
      as.character(unique(alignment_mms$political_alignment)),
      c("non_aligned", "aligned")
    ),
    setequal(
      unique(alignment_differences$direction),
      c("home_within_alignment", "alignment_within_home")
    ),
    nrow(alignment_sensitivity) == 9L,
    setequal(
      unique(alignment_sensitivity$coding),
      c("narrow", "primary", "broad")
    ),
    setequal(
      unique(robustness$weight),
      c("unweighted", "census_margins", "census_vote_2023")
    ),
    setequal(unique(robustness$outcome), c("competence", "advantage")),
    nrow(balance) > 0L,
    identical(advantage_distribution$rating, 0:10),
    sum(advantage_distribution$n) == nrow(conjoint) / 2,
    all(is.finite(main_effects$estimate)),
    all(is.finite(office_mms$estimate)),
    all(is.finite(office_interaction_test$statistic)),
    all(is.finite(office_interaction_test$p.value)),
    all(is.finite(alignment_mms$estimate)),
    all(is.finite(alignment_differences$estimate)),
    all(is.finite(alignment_sensitivity$estimate))
  )
}

build_paper_outputs <- function(
  conjoint_path,
  respondent_path,
  results_dir = "results",
  figures_dir = "../paper_draft/figs"
) {
  reports_dir <- results_dir

  dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

  conjoint <- prepare_conjoint_data(conjoint_path)
  respondent_groups <- prepare_respondent_groups(respondent_path)
  alignment_data <- add_political_alignment(conjoint, respondent_groups)

  office_counts <- conjoint %>%
    distinct(ID, office) %>%
    count(office, name = "respondents") %>%
    mutate(
      office_nice = nice_office(office),
      profiles = respondents * 8L
    )

  main_formula <- competence ~ age + sex + occupation + government + municip + key_issue + office
  office_formula <- competence ~ age + sex + occupation + government + municip * office + key_issue
  alignment_formula <- competence ~ age + sex + occupation + key_issue + office + respondent_gov + municip * political_alignment

  main_fit <- fit_lpm(conjoint, main_formula)
  office_fit <- fit_lpm(conjoint, office_formula)
  alignment_fit <- fit_lpm(alignment_data, alignment_formula)

  main_effects <- collect_main_estimands(main_fit)

  office_mms <- estimate_mm(office_fit, "municip", by = "office") %>%
    transmute(
      office = as.character(office),
      municip = as.character(municip),
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )

  office_interaction <- survey::regTermTest(office_fit, ~municip:office)
  office_interaction_test <- tibble(
    term = "municip:office",
    statistic = as.numeric(office_interaction$Ftest),
    df_num = as.numeric(office_interaction$df),
    df_den = as.numeric(office_interaction$ddf),
    p.value = as.numeric(office_interaction$p)
  )

  alignment_mms <- estimate_mm(
    alignment_fit,
    "municip",
    by = "political_alignment"
  ) %>%
    transmute(
      political_alignment = as.character(political_alignment),
      municip = as.character(municip),
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )

  home_differences <- estimate_reference_difference(
    alignment_fit,
    "municip",
    by = "political_alignment"
  ) %>%
    transmute(
      direction = "home_within_alignment",
      political_alignment = as.character(political_alignment),
      municip = NA_character_,
      contrast,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )

  political_differences <- estimate_reference_difference(
    alignment_fit,
    "political_alignment",
    by = "municip"
  ) %>%
    transmute(
      direction = "alignment_within_home",
      political_alignment = NA_character_,
      municip = as.character(municip),
      contrast,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )

  alignment_differences <- bind_rows(home_differences, political_differences)

  alignment_schemes <- tribble(
    ~coding, ~definition, ~anti_max, ~pro_min,
    "narrow", "0-2 vs 8-10", 2, 8,
    "primary", "0-3 vs 7-10", 3, 7,
    "broad", "0-4 vs 6-10", 4, 6
  )

  alignment_sensitivity <- purrr::pmap_dfr(
    alignment_schemes,
    function(coding, definition, anti_max, pro_min) {
      sensitivity_data <- conjoint %>%
        add_political_alignment(
          prepare_respondent_groups(
            respondent_path,
            anti_max = anti_max,
            pro_min = pro_min
          )
        )
      sensitivity_fit <- fit_lpm(sensitivity_data, alignment_formula)

      estimate_reference_difference(
        sensitivity_fit,
        "political_alignment",
        by = "municip"
      ) %>%
        filter(contrast == "aligned - non_aligned") %>%
        transmute(
          coding,
          definition,
          anti_max,
          pro_min,
          respondents = n_distinct(sensitivity_data$ID),
          profiles = nrow(sensitivity_data),
          municip = as.character(municip),
          contrast,
          estimate,
          std.error,
          conf.low,
          conf.high,
          p.value
        )
    }
  )

  robustness_grid <- tidyr::crossing(
    outcome = c("competence", "advantage"),
    source_weight = c("unweighted", "waga1", "waga2")
  ) %>%
    filter(outcome == "competence" | source_weight == "unweighted")

  robustness <- purrr::pmap_dfr(
    robustness_grid,
    function(outcome, source_weight) {
      formula <- reformulate(
        c("age", "sex", "occupation", "government", "municip", "key_issue", "office"),
        response = outcome
      )
      fit <- fit_lpm(conjoint, formula, source_weight)

      bind_rows(
        estimate_reference_difference(fit, "government") %>%
          mutate(attribute = "government"),
        estimate_reference_difference(fit, "municip") %>%
          mutate(attribute = "municip")
      ) %>%
        transmute(
          outcome,
          weight = recode(
            source_weight,
            unweighted = "unweighted",
            waga1 = "census_margins",
            waga2 = "census_vote_2023"
          ),
          attribute,
          contrast,
          estimate,
          std.error,
          conf.low,
          conf.high,
          p.value
        )
    }
  )

  balance <- conjoint %>%
    mutate(across(c(age, sex, occupation, government, municip, key_issue), as.character)) %>%
    pivot_longer(
      cols = c(age, sex, occupation, government, municip, key_issue),
      names_to = "attribute",
      values_to = "level"
    ) %>%
    count(attribute, level, office, name = "n") %>%
    group_by(attribute, office) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup() %>%
    mutate(office = as.character(office))

  advantage_distribution <- conjoint %>%
    filter(competence == 1L) %>%
    count(rating = as.integer(advantage), name = "n") %>%
    complete(rating = 0:10, fill = list(n = 0L)) %>%
    arrange(rating) %>%
    mutate(percent = 100 * n / sum(n))

  full_model_estimates <- broom::tidy(main_fit, conf.int = TRUE) %>%
    transmute(
      term,
      term_nice = nice_model_term(term),
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )

  validate_results(
    conjoint,
    alignment_data,
    main_effects,
    office_mms,
    office_interaction_test,
    alignment_mms,
    alignment_differences,
    alignment_sensitivity,
    robustness,
    balance,
    advantage_distribution
  )

  write_csv(
    office_counts %>% select(office = office, office_nice, respondents, profiles),
    file.path(reports_dir, "paper_design_summary.csv")
  )
  write_csv(
    main_effects %>%
      select(
        attribute, attribute_nice, level, level_nice, estimand, contrast,
        is_reference, estimate, std.error, conf.low, conf.high, p.value
      ),
    file.path(reports_dir, "paper_main_effects.csv")
  )
  write_csv(office_mms, file.path(reports_dir, "paper_municip_by_office.csv"))
  write_csv(
    office_interaction_test,
    file.path(reports_dir, "paper_office_interaction_test.csv")
  )
  write_csv(alignment_mms, file.path(reports_dir, "paper_alignment_marginal_means.csv"))
  write_csv(alignment_differences, file.path(reports_dir, "paper_alignment_differences.csv"))
  write_csv(
    alignment_sensitivity,
    file.path(reports_dir, "paper_alignment_sensitivity.csv")
  )
  write_csv(robustness, file.path(reports_dir, "paper_robustness.csv"))
  write_csv(balance, file.path(reports_dir, "paper_balance.csv"))
  write_csv(
    advantage_distribution,
    file.path(reports_dir, "paper_advantage_distribution.csv")
  )
  write_csv(full_model_estimates, file.path(reports_dir, "paper_full_model_estimates.csv"))

  main_plot <- make_main_effect_plot(main_effects)
  office_plot <- make_office_plot(office_mms)
  interaction_plot <- make_interaction_plot(alignment_mms, alignment_differences)

  ggsave(
    file.path(figures_dir, "paper_main_effects.png"),
    main_plot,
    width = 9,
    height = 8.5,
    dpi = 600
  )
  ggsave(
    file.path(figures_dir, "paper_municip_by_office.png"),
    office_plot,
    width = 8,
    height = 4.8,
    dpi = 600
  )
  ggsave(
    file.path(figures_dir, "paper_cue_heterogeneity.png"),
    interaction_plot,
    width = 9,
    height = 7.2,
    dpi = 600
  )

  output_files <- c(
    file.path(
      reports_dir,
      c(
        "paper_design_summary.csv",
        "paper_main_effects.csv",
        "paper_municip_by_office.csv",
        "paper_office_interaction_test.csv",
        "paper_alignment_marginal_means.csv",
        "paper_alignment_differences.csv",
        "paper_alignment_sensitivity.csv",
        "paper_robustness.csv",
        "paper_balance.csv",
        "paper_advantage_distribution.csv",
        "paper_full_model_estimates.csv"
      )
    ),
    file.path(
      figures_dir,
      c(
        "paper_main_effects.png",
        "paper_municip_by_office.png",
        "paper_cue_heterogeneity.png"
      )
    )
  )

  stopifnot(all(file.exists(output_files)))
  output_files
}
