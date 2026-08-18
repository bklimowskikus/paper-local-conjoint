# Local Roots Within a National Political Divide

This directory reproduces the linear probability model results and figures for the Quarto paper "Local Roots Within a National Political Divide: Evidence from a Split-Ballot Candidate Conjoint in Poland."

## Contents

- `data/` contains the two raw Stata files used by the analysis.
- `R/analysis.R` contains the data preparation, models, estimands, checks, and plots.
- `_targets.R` defines the analysis workflow.
- `results/` contains numerical outputs.
- `../paper_draft/v2/` contains the revised manuscript and receives the three generated figures.
- `tests/check_reproduction.R` checks the analysis outputs and manuscript inputs.

The package excludes exploratory Bayesian, causal-forest, and pairwise-interaction analyses that are not required for the manuscript.

## Requirements

- R 4.3.3 or a compatible later release
- Quarto

Exact R package versions are recorded in `renv.lock`.

## Reproduce the paper

Run these commands from this directory:

~~~bash
Rscript -e 'renv::restore()'
Rscript -e 'targets::tar_make()'
~~~

Then run the artifact check:

~~~bash
Rscript tests/check_reproduction.R
~~~

Numerical estimates are written to `results/`; figures are written to `../paper_draft/v2/figs/`. Render the paper separately with `quarto render index.qmd` from `paper_draft/v2/`.

Check pipeline status with:

~~~r
targets::tar_outdated()
~~~

An empty result from targets::tar_outdated() means all targets are current.

## Analysis

The main outcome is the profile-level indicator that a candidate was selected as more competent. The analysis uses unweighted linear probability models with respondent-clustered standard errors. It reports marginal means, AMCEs, office-specific municipal-roots estimates, an omnibus office-interaction test, a binary government-alignment comparison among clearly positioned cases, alignment-cutoff sensitivity, weighted checks, and an alternative advantage-scale outcome.

The primary alignment comparison excludes neutral candidate profiles and respondents scoring 4--6 on the 0--10 government-attitude scale. The sensitivity output repeats the analysis with narrower and broader respondent cutoffs. The pipeline stops when expected sample sizes, estimand levels, output uniqueness, or generated files do not match the analysis contract.

## Data

See `data/README.md` for the input files and variables used. Both source datasets may be redistributed with this reproduction package.
