# Local Roots Within a National Political Divide

This directory reproduces the linear probability model results and figures for the Quarto paper "Local Roots Within a National Political Divide: Evidence from a Split-Ballot Candidate Conjoint in Poland."

## Contents

- `data/` contains the two raw Stata files used by the analysis.
- `R/analysis.R` contains the data preparation, models, estimands, checks, and plots.
- `_targets.R` defines the analysis workflow.
- `results/` contains numerical outputs.
- `results/figures/` contains the four generated figures.
- `tests/check_reproduction.R` checks the analysis outputs and generated figures.

The package excludes exploratory Bayesian, causal-forest, and pairwise-interaction analyses that are not required for the manuscript.

## Requirements

- R 4.3.3 (or a compatible later release).
- The `renv` package. If it is not already installed, run:

  ~~~r
  install.packages("renv")
  ~~~

- The supplied raw inputs `data/conjoint_1fala.dta` and
  `data/PLSW_1-2fala_final.dta` (described in `data/README.md`).
- Internet access the first time `renv::restore()` installs the locked R
  packages, plus write access to `results/`.

Quarto is not required here because this folder does not render the manuscript.

Exact R package versions are recorded in `renv.lock`.

## Reproduce analysis artifacts

Run these commands from this directory:

~~~bash
Rscript -e 'renv::restore()'
Rscript -e 'targets::tar_make()'
~~~

Then run the artifact check:

~~~bash
Rscript tests/check_reproduction.R
~~~

Numerical estimates are written to `results/`; four figures are written to `results/figures/`. This folder does not render or validate the manuscript.

## Working with `targets`

`targets` records completed work in `_targets/`. `tar_make()` rebuilds only targets whose inputs or code have changed.

List the three pipeline targets and the code they run:

~~~r
targets::tar_manifest(fields = c("name", "command"))
~~~

Check what would run before rebuilding:

~~~r
targets::tar_outdated()
~~~

An empty result from targets::tar_outdated() means all targets are current.

To regenerate the final analysis outputs deliberately, invalidate that target and rebuild:

~~~r
targets::tar_invalidate(analysis_outputs)
targets::tar_make()
~~~

Inspect the files recorded for the completed analysis target:

~~~r
targets::tar_read(analysis_outputs)
~~~

## Output map

The table maps generated CSVs to the figures and tables in the paper. Manuscript
labels are shown in parentheses because displayed table numbers can change when
the paper is rendered.

| Reproduction output | Generated figure | Paper table or use |
|:---|:---|:---|
| `paper_main_effects.csv` | Marginal means and AMCEs (`paper_main_effects.png`) | Figure `fig-main-effects`; its underlying pooled coefficients are in `paper_full_model_estimates.csv` and the pooled-model table (`tbl-model-estimates`). |
| `paper_dominance.csv`, `paper_dominance_comparison.csv` | General dominance weights (`paper_dominance.png`) | Figure `fig-dominance`; no separate paper table. |
| `paper_municip_by_office.csv` | Municipal roots by office (`paper_municip_by_office.png`) | Figure `fig-office`; no separate paper table. `paper_office_interaction_test.csv` and `paper_office_pairwise.csv` support the text interpretation. |
| `paper_alignment_marginal_means.csv`, `paper_alignment_differences.csv`, `paper_alignment_cross_profile.csv` | Municipal roots and political alignment (`paper_cue_heterogeneity.png`) | Figure `fig-heterogeneity`; pairwise-contrast table (`tbl-figure3-pairwise`) uses `paper_alignment_differences.csv`. |
| `paper_balance.csv` | — | Randomized attribute-level shares by office (`tbl-balance`). |
| `paper_advantage_distribution.csv` | — | Distribution of the selected profile's advantage rating (`tbl-advantage-distribution`). |
| `paper_alignment_sensitivity.csv` | — | Alignment-coding sensitivity (`tbl-alignment-sensitivity`). |
| `paper_alignment_interaction_tests.csv` | — | Interaction robustness (`tbl-interaction-robustness`). |
| `paper_robustness.csv` | — | Robustness of focal cue estimates (`tbl-robustness`). |
| `paper_design_summary.csv`, `paper_respondent_balance.csv` | — | Sample-description and diagnostic outputs used in text and checks; no standalone paper table. |

## Analysis

The main outcome is the profile-level indicator that a candidate was selected as more competent. The analysis uses unweighted linear probability models with respondent-clustered standard errors. It reports marginal means, AMCEs, office-specific municipal-roots estimates, an omnibus office-interaction test, a binary government-alignment comparison among clearly positioned cases, alignment-cutoff sensitivity, weighted checks, and an alternative advantage-scale outcome.

The primary alignment comparison excludes neutral candidate profiles and respondents scoring 4--6 on the 0--10 government-attitude scale. The sensitivity output repeats the analysis with narrower and broader respondent cutoffs. The pipeline stops when expected sample sizes, estimand levels, output uniqueness, or generated files do not match the analysis contract.

## Data

See `data/README.md` for the input files and variables used. Both source datasets may be redistributed with this reproduction package.
