# Local Roots and National Political Alignment

This repository contains an English-language paper draft and the analysis needed to reproduce its empirical results.

- `paper_draft/` contains the Quarto manuscript, bibliography, style, and figures.
- `reproduction/` contains the source data, R analysis, numerical results, locked R environment, and verification check.

## Reproduce

From `reproduction/`:

```bash
Rscript -e 'renv::restore()'
Rscript -e 'targets::tar_make()'
Rscript tests/check_reproduction.R
```

Then render the manuscript from `paper_draft/`:

```bash
quarto render index.qmd
```

See `reproduction/README.md` for the estimands and data details.
