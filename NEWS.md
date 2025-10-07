# easyStatTools 0.1.0

* Initial CRAN submission.

# easyStatTools 1.1.0

## New Function

- `my_t_test_helper()` supports new argument `correction`.
  - allows to deactivate correction for multiple testing (`correction = FALSE`).
  - standard is a activated correction (`correction = TRUE`).

# easyStatTools 2.0.0

## Major changes
- Variance homogeneity check now uses **Brown–Forsythe test** instead of Levene’s test in both `my_t_test_helper` and `my_anova_helper`.

## New features
- `my_anova_helper()` gains a new argument `ref.group`:
  - If `NULL` (default), all group combinations are compared.
  - If set to a group (e.g. `"control"`), only comparisons against this reference group are performed.
  - Depending on the ANOVA type, specific post-hoc tests are applied:
    - Classical ANOVA → Dunnett’s test  
    - Welch ANOVA → Dunnett’s T3 test (via **PMCMRplus**)  
    - Kruskal-Wallis → Dunn test (restricted to ref.group)

## Other
- Documentation and README updated to reflect the new methods.

#easStatTools 2.1.0
- bug fixes

#easyStatTools 2.1.1
- more bug fixes