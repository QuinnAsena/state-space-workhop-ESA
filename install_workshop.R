if (!require("pacman")) install.packages("pacman", repos="http://cran.r-project.org")
pacman::p_load(rmarkdown, languageserver, ggplot2, dplyr, tidyr, devtools, stringr, scales,
               RcppArmadillo, minqa, matrixStats, numDeriv, mvtnorm, multinomialTS) # Install & load packages
# devtools::install_github("https://github.com/QuinnAsena/multinomialTS")

# install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.zip", repos = NULL, type = "win.binary")

# install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.tgz", repos = NULL, type = "mac.binary")
