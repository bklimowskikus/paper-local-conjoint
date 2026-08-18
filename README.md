# Local Roots Within a National Political Divide

This repository contains an English-language paper draft and the analysis needed to reproduce its empirical results.

- `paper_draft/v1/` contains the baseline manuscript and `paper_draft/v2/` the revised manuscript.
- `reproduction/` contains the source data, R analysis, numerical results, locked R environment, and verification check.

## Reproduce

From `reproduction/`:

```bash
Rscript -e 'renv::restore()'
Rscript -e 'targets::tar_make()'
Rscript tests/check_reproduction.R
```

Then render the revised manuscript from `paper_draft/v2/`:

```bash
quarto render index.qmd
```

See `reproduction/README.md` for the estimands and data details.
