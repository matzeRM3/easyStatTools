# easyStatTools

**A lightweight R package for automated statistical testing**  
with test recommendations based on normality and variance assumptions.

---

## Installation

Install the package directly from GitHub:

```r
install.packages("devtools")  # if not already installed
devtools::install_github("matzeRM3/easyStatTools")
```
### 
Functions

`my_anova_helper(formula, data, ask = TRUE)`

Performs:
- Shapiro-Wilk test (normality per group)
- Levene’s test (homogeneity of variance)

Based on assumptions, recommends and runs:
- Classical ANOVA + Tukey post-hoc
- Welch ANOVA + Games-Howell post-hoc
- Kruskal-Wallis + Dunn post-hoc

Includes interactive dialog (`ask = TRUE`) or silent auto-run mode (`ask = FALSE`).

**Example:**

```r
result <- my_anova_helper(foldchange ~ group, data = mydata, ask = FALSE)
print(result)
```

`my_t_test_helper(formula, data, ref.group = NULL, paired = FALSE, ask = TRUE, correction = TRUE)`

Performs:

Shapiro-Wilk test (normality)

Levene’s test (variance homogeneity)

Recommends and runs:

Student’s t-test

Welch’s t-test

Wilcoxon rank-sum test

Handles both 2-group and multi-group comparisons.

For >2 groups, requires ref.group to compare all levels against the control.

Applies multiple testing correction (Holm, Bonferroni, or BH) by default.
Set correction = FALSE to skip p-value adjustment.

**Examples:**

```r
# Two groups
result <- my_t_test_helper(foldchange ~ treatment, data = mydata, ask = FALSE)

# Multiple groups vs control
result <- my_t_test_helper(foldchange ~ group, data = mydata, ref.group = "ctrl", ask = FALSE)
```
###
Output

Both functions return a tibble with:

group1, group2

estimate, conf.low, conf.high

p, p.adj, p.adj.signif (****, ***, **, *, ns)

Can be used with ggplot2::geom_signif() for annotated bar plots.

###
Dependencies

rstatix

car

dplyr

multcompView

##
Feedback

Open an issue at:
https://github.com/matzeRM3/easyStatTools/issues

##
License

MIT © Matthias Plath
