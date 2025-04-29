easyStatTools R Package
=======================

How to install locally:
1. Open RStudio and set working directory to this folder.
2. Run:
   install.packages(c("devtools", "roxygen2"))
   library(devtools)
   document()
   install()
3. Load the package:
   library(easyStatTools)

Function usage:
   my_anova_helper(y ~ x, dataframe)
