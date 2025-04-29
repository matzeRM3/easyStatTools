#' ANOVA Helper: Automated Test Selection and Post-hoc Analysis
#'
#' Checks for normality (Shapiro-Wilk) and homogeneity of variance (Levene),
#' recommends the appropriate ANOVA and post-hoc test, and performs the analysis.
#'
#' @param formula Model formula (e.g. value ~ group).
#' @param data Dataframe containing the variables.
#' @param ask Logical. Should user be asked whether to proceed (default TRUE)?
#'
#' @return Post-hoc results as dataframe (tibble), with rstatix-style significance stars in 'p.adj.signif'.
#' @export
#' @importFrom car leveneTest
#' @importFrom rstatix anova_test welch_anova_test kruskal_test tukey_hsd games_howell_test dunn_test
#' @importFrom dplyr group_by summarise mutate sym
#'
#' @examples
#' my_anova_helper(value ~ group, mydata, ask = FALSE)
my_anova_helper <- function(formula, data, ask = TRUE) {
  if (!requireNamespace("car", quietly = TRUE)) stop("Package 'car' is required.")
  if (!requireNamespace("rstatix", quietly = TRUE)) stop("Package 'rstatix' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  
  library(car)
  library(rstatix)
  library(dplyr)
  
  vars <- all.vars(formula)
  response <- vars[1]
  group <- vars[2]
  
  # Shapiro-Wilk test per group
  cat("Shapiro-Wilk test (normality per group):\n")
  shap_results <- data %>%
    group_by(!!sym(group)) %>%
    summarise(p = shapiro.test(.data[[response]])$p.value)
  print(shap_results)
  normal <- all(shap_results$p > 0.05)
  
  # Levene's test
  lev <- car::leveneTest(formula, data)
  cat("\nLevene's test (homogeneity of variance):\n")
  print(lev)
  lev_p <- lev$"Pr(>F)"[1]
  homo <- lev_p > 0.05
  
  # Recommendation
  if (normal & homo) {
    rec_anova <- "classical ANOVA"
    rec_posthoc <- "Tukey"
  } else if (normal & !homo) {
    rec_anova <- "Welch ANOVA"
    rec_posthoc <- "Games-Howell"
  } else {
    rec_anova <- "Kruskal-Wallis"
    rec_posthoc <- "Dunn"
  }
  cat("\nBased on the results, following tests are recommended:\n")
  cat(rec_anova, "+", rec_posthoc, "post-hoc\n")
  
  # User dialog (nur wenn ask == TRUE)
  if (ask) {
    repeat {
      user <- readline("Do you want to proceed? (y=yes/n=no/c=custom): ")
      if (user %in% c("y", "n", "c")) break
    }
    if (user == "n") return(invisible(NULL))
    
    if (user == "c") {
      cat("Choose ANOVA type:\n1: classical ANOVA\n2: Welch ANOVA\n3: Kruskal-Wallis\n")
      atype <- as.integer(readline("Choose number: "))
      anova_types <- c("classical ANOVA", "Welch ANOVA", "Kruskal-Wallis")
      rec_anova <- anova_types[atype]
      cat("Choose post-hoc:\n1: Tukey\n2: Games-Howell\n3: Dunn\n")
      ptype <- as.integer(readline("Choose number: "))
      posthoc_types <- c("Tukey", "Games-Howell", "Dunn")
      rec_posthoc <- posthoc_types[ptype]
    }
  }
  
  # Perform analysis and post-hoc
  if (rec_anova == "classical ANOVA") {
    res_aov <- rstatix::anova_test(data = data, formula = formula, detailed = TRUE)
    res_aov <- as.data.frame(res_aov)
    print(res_aov)
    if (rec_posthoc == "Tukey") {
      tuk <- rstatix::tukey_hsd(data, formula)
      return(tuk)
    }
  }
  if (rec_anova == "Welch ANOVA") {
    welch <- rstatix::welch_anova_test(data = data, formula = formula)
    print(welch)
    if (rec_posthoc == "Games-Howell") {
      gh <- rstatix::games_howell_test(data = data, formula = formula)
      return(gh)
    }
  }
  if (rec_anova == "Kruskal-Wallis") {
    kw <- rstatix::kruskal_test(data = data, formula = formula)
    print(kw)
    if (rec_posthoc == "Dunn") {
      dunn <- rstatix::dunn_test(data = data, formula = formula, detailed = TRUE)
      return(dunn)
    }
  }
  cat("Selected combination not supported.\n")
}
