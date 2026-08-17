# Data inputs

The pipeline reads two unmodified Stata files.

## conjoint_1fala.dta

Wave 1 candidate-conjoint profiles. The analysis uses:

- ID: respondent identifier;
- profile: profile identifier;
- competence: binary indicator for the profile selected as more competent for the office;
- advantage: strength of the selected profile's advantage;
- Census-margin post-stratification weight (source column `waga1`);
- Census and reported 2023-vote post-stratification weight (source column `waga2`);
- age, sex, occupation, government, municip, and key_issue: randomized candidate attributes;
- conjoint_split: randomized office prompt.

The validated analysis contains 17,656 profiles from 2,207 respondents.

## PLSW_1-2fala_final.dta

Respondent-level survey file. The analysis uses ID to join respondents and nat_govopp to classify attitudes toward the national government. The primary alignment comparison uses scores 0--3 and 7--10 and excludes neutral candidate profiles; sensitivity checks vary the respondent cutoffs.

The analysis never overwrites either input file.

Both input files may be redistributed with this reproduction package.

## PLSW Raport.md

Polish-language technical documentation for the survey, sampling, fieldwork, weights, questionnaire, and conjoint design used in the manuscript.
