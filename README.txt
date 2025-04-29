# easyStatTools

**A lightweight R package for automated statistical testing**  
with recommendations based on data assumptions (normality, variance homogeneity).

---

## 🔧 Installation

Install the package directly from GitHub:

```r
install.packages("devtools")  # if not already installed
devtools::install_github("matzeRM3/easyStatTools")

Functions
my_anova_helper(formula, data, ask = TRUE)
Performs:

Shapiro-Wilk test (normality per group)

Levene’s test (homogeneity of variance)

Recommends and runs:

classical ANOVA + Tukey

Welch ANOVA + Games-Howell

Kruskal-Wallis + Dunn

Includes an interactive dialog (ask = TRUE) or auto-run mode (ask = FALSE).

Example:

result <- my_anova_helper(foldchange ~ group, mydata, ask = FALSE)
print(result)  # post-hoc tibble with p-values and stars

my_t_test_helper(formula, data, ref.group = NULL, paired = FALSE, ask = TRUE)
Performs:

Shapiro-Wilk + Levene tests

Recommends:

Student t-test

Welch t-test

Wilcoxon rank-sum test

Applies multiple testing correction (Holm, Bonferroni, BH)

If more than 2 groups are found, all are compared to a reference group (ref.group required).

Example with 2 groups:
result <- my_t_test_helper(foldchange ~ treatment, data = mydata, ask = FALSE)

Example with >2 groups:
result <- my_t_test_helper(foldchange ~ group, data = mydata, ref.group = "ctrl", ask = FALSE)

Both functions return a tibble with these columns:

group1 / group2

estimate / conf.low / conf.high

p / p.adj / p.adj.signif
→ significance stars according to p.adj

Use geom_signif() in ggplot2 with results$p.adj.signif for annotated bar plots.

Dependencies
The package depends on:

rstatix

car

dplyr

multcompView

Feedback
Open an issue at:
https://github.com/matzeRM3/easyStatTools/issues

License
MIT © Matthias Plath
---
