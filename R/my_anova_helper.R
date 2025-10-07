#' ANOVA Helper: Automated Test Selection and Post-hoc Analysis
#'
#' Checks for normality (Shapiro-Wilk) and homogeneity of variance (Brown-Forsythe test),
#' recommends the appropriate ANOVA and post-hoc test, and performs the analysis.
#'
#' @param formula Model formula (e.g. value ~ group).
#' @param data Dataframe containing the variables.
#' @param ask Logical. Should user be asked whether to proceed (default TRUE)?
#' @param ref.group Optional. Name of reference group. If NULL, all pairwise comparisons are performed.
#'
#' @return Post-hoc results as dataframe (tibble), with rstatix-style significance stars in 'p.adj.signif'.
#' @export
#' @importFrom car leveneTest
#' @importFrom rstatix anova_test welch_anova_test kruskal_test tukey_hsd games_howell_test dunn_test
#' @importFrom PMCMRplus dunnettT3Test
#' @importFrom dplyr group_by summarise mutate sym filter
#' @importFrom stats relevel shapiro.test symnum
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' set.seed(123)
#' demo <- data.frame(
#'   value = c(rnorm(10, 5), rnorm(10, 6), rnorm(10, 7)),
#'   group = rep(c("ctrl", "A", "B"), each = 10)
#' )
#' my_anova_helper(value ~ group, demo, ask = FALSE, ref.group = "ctrl")
my_anova_helper <- function(formula, data, ask = TRUE, ref.group = NULL) {
  if (!requireNamespace("car", quietly = TRUE)) stop("Package 'car' is required.")
  if (!requireNamespace("rstatix", quietly = TRUE)) stop("Package 'rstatix' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("PMCMRplus", quietly = TRUE)) stop("Package 'PMCMRplus' is required.")
  
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
    summarise(p = shapiro.test(.data[[response]])$p.value, .groups = "drop")
  print(shap_results)
  normal <- all(shap_results$p > 0.05)
  
  # Brown-Forsythe test (variance homogeneity, median-centered Levene)
  lev <- car::leveneTest(formula, data, center = "median")
  cat("\nBrown-Forsythe test (homogeneity of variance):\n")
  print(lev)
  lev_p <- lev$"Pr(>F)"[1]
  homo <- lev_p > 0.05
  
  # Recommendation
  if (normal & homo) {
    rec_anova <- "classical ANOVA"
    rec_posthoc <- if (is.null(ref.group)) "Tukey" else "Dunnett"
  } else if (normal & !homo) {
    rec_anova <- "Welch ANOVA"
    rec_posthoc <- if (is.null(ref.group)) "Games-Howell" else "DunnettT3"
  } else {
    rec_anova <- "Kruskal-Wallis"
    rec_posthoc <- "Dunn"  # Dunn bleibt, aber filter bei ref.group
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
      cat("Choose post-hoc:\n1: Tukey\n2: Games-Howell\n3: Dunnett\n4: DunnettT3\n5: Dunn\n")
      ptype <- as.integer(readline("Choose number: "))
      posthoc_types <- c("Tukey", "Games-Howell", "Dunnett", "DunnettT3", "Dunn")
      rec_posthoc <- posthoc_types[ptype]
    }
  }
  
  # --- Perform analysis and post-hoc ---
  if (rec_anova == "classical ANOVA") {
    res_aov <- rstatix::anova_test(data = data, formula = formula, detailed = TRUE)
    print(as.data.frame(res_aov))
    if (rec_posthoc == "Tukey") {
      return(rstatix::tukey_hsd(data, formula))
    }
    if (rec_posthoc == "Dunnett") {
      data[[group]] <- relevel(factor(data[[group]]), ref = ref.group)
      dun <- PMCMRplus::dunnettTest(formula, data = data)
      posthoc <- as.data.frame(summary(dun)$p.value) |> 
        tibble::rownames_to_column("Comparison") |>
        tidyr::pivot_longer(-Comparison, names_to = "Group", values_to = "p.adj") |>
        dplyr::filter(Comparison == ref.group | Group == ref.group)
      posthoc$p.adj.signif <- symnum(
        posthoc$p.adj,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
      return(posthoc)
    }
    
  }
  
  if (rec_anova == "Welch ANOVA") {
    welch <- rstatix::welch_anova_test(data = data, formula = formula)
    print(welch)
    if (rec_posthoc == "Games-Howell") {
      return(rstatix::games_howell_test(data = data, formula = formula))
    }
    if (rec_posthoc == "DunnettT3") {
      data[[group]] <- relevel(factor(data[[group]]), ref = ref.group)
      dun <- PMCMRplus::dunnettT3Test(formula, data = data)
      posthoc <- as.data.frame(summary(dun)$p.value) |> 
        tibble::rownames_to_column("Comparison") |>
        tidyr::pivot_longer(-Comparison, names_to = "Group", values_to = "p.adj") |>
        dplyr::filter(Comparison == ref.group | Group == ref.group)
      posthoc$p.adj.signif <- symnum(
        posthoc$p.adj,
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
        symbols = c("****", "***", "**", "*", "ns")
      )
      return(posthoc)
    }
    
  }
  
  if (rec_anova == "Kruskal-Wallis") {
    kw <- rstatix::kruskal_test(data = data, formula = formula)
    print(kw)
    dunn <- rstatix::dunn_test(data = data, formula = formula, detailed = TRUE)
    if (!is.null(ref.group)) {
      dunn <- dunn %>% filter(group1 == ref.group | group2 == ref.group)
    }
    return(dunn)
  }
  
  cat("Selected combination not supported.\n")
}
