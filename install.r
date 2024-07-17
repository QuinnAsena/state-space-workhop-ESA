if (!require("pacman")) install.packages("pacman", repos="http://cran.r-project.org")
pacman::p_load(rmarkdown, languageserver, ggplot2, dplyr, tidyr, devtools, stringr, scales) # Install & load packages
devtools::install_github("https://github.com/QuinnAsena/multinomialTS")
