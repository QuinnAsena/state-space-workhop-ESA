if (!require("multinomialTS")) {
  stop("
    Required dependency multinomialTS.
    See the README: https://github.com/QuinnAsena/state-space-workhop-ESA/tree/main#welcome-to-the-multinomialts-workshop ; 
    or the instruction comments below in the install_workshop.R file (this file),
    or try my workshop_autoinstall.R (which may or may not work...)"
  )
} else {
if (!require("pacman")) install.packages("pacman", repos="http://cran.r-project.org")
pacman::p_load(neotoma2, rmarkdown, languageserver, ggplot2, dplyr, tidyr, devtools, stringr, scales,
               RcppArmadillo, minqa, matrixStats, numDeriv, mvtnorm, multinomialTS) # Install & load packages
}


# If necessary to install multinomialTS from GitHub Directly
# Requires xcode-select on mac
# Requires Rtools44 on windows
# devtools::install_github("https://github.com/QuinnAsena/multinomialTS")

# To install binary files for windows, does not require Rtools44
# Requires R 4.4 or greater
# install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.zip", repos = NULL, type = "win.binary")

# To install binary files for mac, does not require xcode-select
# Requires R 4.4 or greater
# install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.tgz", repos = NULL, type = "mac.binary")


# neotoma2
# If necessary to install neotoma2 from github directly:
# devtools::install_github('NeotomaDB/neotoma2', build_vignettes = TRUE)
