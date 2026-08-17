# AGENTS.md

## Repository scope

This repository has two working directories:

- `paper_draft/`: the English-language Quarto manuscript, bibliography, style, and figures.
- `reproduction/`: the source data, one R analysis pipeline, numerical results, and verification check.

Do not create a second manuscript or a parallel analysis path.

## Paper rules

- Write concise, neutral political-science prose in English.
- Treat municipal residence as the randomized profile signal; do not claim that the experiment identifies campaign effort, personal acquaintance, or social-network mechanisms.
- Distinguish statistical uncertainty from substantive magnitude.
- Do not claim office or subgroup differences without estimates and uncertainty.
- Ground empirical claims in `reproduction/results/` and literature claims in `paper_draft/references.bib`.

## Analysis rules

- Use the existing unweighted linear probability models with respondent-clustered standard errors as the main specification.
- Do not overwrite the raw files in `reproduction/data/` or silently change estimands.
- Keep new analysis code minimal and directly connected to a paper claim, table, or figure.
- Use deterministic seeds for any randomized computation.

## Verification

After changing analysis code, run from `reproduction/`:

```bash
Rscript -e 'targets::tar_make()'
Rscript tests/check_reproduction.R
```

After changing the manuscript, run from `paper_draft/`:

```bash
quarto render index.qmd
```
