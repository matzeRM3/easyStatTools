#' t-Test Helper: Automated Group-vs-Control or Two-Group Comparison with Multiple Testing Correction
#'
#' Checks for normality (Shapiro-Wilk) and homogeneity of variance (Levene),
#' recommends the appropriate t-test or Wilcoxon test and multiple testing correction,
#' compares all groups against a reference group (if >2), or compares two groups,
#' and returns a result table with adjusted p-values and significance stars.
#'
#' @param formula Model formula (e.g. value ~ group).
#' @param data Dataframe containing the variables.
#' @param ref.group (Optional) The name of the reference (control) group (as string) for multiple group comparisons.
#' @param paired Logical. Should a paired test be performed? Default: FALSE.
#' @param ask Logical. Should the user be asked whether to proceed? Default: TRUE.
#' @param correction Logical. Should p-value adjustment for multiple testing be applied? Default: TRUE.
#'
#' @return Dataframe with test results and significance stars.
#' @export
#' @importFrom car leveneTest
#' @importFrom rstatix t_test wilcox_test adjust_pvalue add_significance
#' @importFrom dplyr group_by summarise sym
#'
#' @examples
#' my_t_test_helper(foldchange ~ group, data = df, ref.group = "ctrl", ask = FALSE, correction = TRUE)
my_t_test_helper <- function(formula, data, ref.group = NULL, paired = FALSE, ask = TRUE, correction = TRUE) {
  if (!requireNamespace("car", quietly = TRUE)) stop("Package 'car' is required.")
  if (!requireNamespace("rstatix", quietly = TRUE)) stop("Package 'rstatix' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  
  library(car)
  library(rstatix)
  library(dplyr)
  
  vars <- all.vars(formula)
  response <- vars[1]
  group <- vars[2]
  data[[group]] <- as.factor(data[[group]])
  
  groups <- unique(as.character(data[[group]]))
  n_groups <- length(groups)
  
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
  
  # Empfehlung/Testauswahl
  if (normal & homo) {
    rec_test <- "Student"
    rec_corr <- ifelse(n_groups == 2 || !correction, "none", "holm")
  } else if (normal & !homo) {
    rec_test <- "Welch"
    rec_corr <- ifelse(n_groups == 2 || !correction, "none", "holm")
  } else {
    rec_test <- "Wilcoxon"
    rec_corr <- ifelse(n_groups == 2 || !correction, "none", "BH")
  }
  
  
  if (n_groups == 2) {
    cat("\nOnly two groups detected: No correction for multiple testing required.\n")
  }
  cat("\nBased on the results, following test/correction is recommended:\n")
  cat(rec_test, "test +", ifelse(rec_corr == "none", "no correction", paste(rec_corr, "correction")), "\n")
  
  # User dialog (nur wenn ask == TRUE)
  if (ask) {
    repeat {
      user <- readline("Do you want to proceed? (y=yes/n=no/c=custom): ")
      if (user %in% c("y", "n", "c")) break
    }
    if (user == "n") return(invisible(NULL))
    
    if (user == "c") {
      cat("Choose test:\n1: Student t-test\n2: Welch t-test\n3: Wilcoxon rank-sum test\n")
      ttype <- as.integer(readline("Choose number: "))
      test_types <- c("Student", "Welch", "Wilcoxon")
      rec_test <- test_types[ttype]
      cat("Choose correction:\n1: None\n2: Bonferroni\n3: Holm\n4: Benjamini-Hochberg (BH)\n")
      ctype <- as.integer(readline("Choose number: "))
      corr_types <- c("none", "bonferroni", "holm", "BH")
      rec_corr <- corr_types[ctype]
    }
  }
  
  # Durchführung
  if (n_groups == 2) {
    if (rec_test %in% c("Student", "Welch")) {
      res <- rstatix::t_test(data, formula, var.equal = (rec_test == "Student"), paired = paired)
    } else if (rec_test == "Wilcoxon") {
      res <- rstatix::wilcox_test(data, formula, paired = paired)
    } else {
      stop("Unknown test type selected.")
    }
    
    if (correction && rec_corr != "none") {
      res <- res %>%
        adjust_pvalue(method = rec_corr)
      res$p.adj.signif <- symnum(
        res$p.adj,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
    } else {
      res$p.adj <- res$p
      res$p.adj.signif <- symnum(
        res$p,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
    }
    return(res)
  } else {
    if (is.null(ref.group)) stop("Please provide 'ref.group' (name of control group) for multiple group comparison.")
    if (rec_test %in% c("Student", "Welch")) {
      res <- rstatix::t_test(data, formula, ref.group = ref.group, var.equal = (rec_test == "Student"), paired = paired)
    } else if (rec_test == "Wilcoxon") {
      res <- rstatix::wilcox_test(data, formula, ref.group = ref.group, paired = paired)
    } else {
      stop("Unknown test type selected.")
    }
    
    if (correction && rec_corr != "none") {
      res <- res %>%
        adjust_pvalue(method = rec_corr)
      res$p.adj.signif <- symnum(
        res$p.adj,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
    } else {
      res$p.adj <- res$p
      res$p.adj.signif <- symnum(
        res$p,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
    }
    return(res)
  }
  
  cat("Selected combination not supported.\n")
}
