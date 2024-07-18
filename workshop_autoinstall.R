if (!require("remotes")) install.packages("remotes", repos="http://cran.r-project.org")
# Check R version
if (compareVersion(paste(getRversion()), "4.4.0") < 0) {
  stop("
  R version needs to be 4.4 or higher. \n
  Would you like to try the binder?
  https://mybinder.org/v2/gh/QuinnAsena/state-space-workhop-ESA/HEAD?urlpath=rstudio \n
  Alternatively, try updating to R 4.4.1, and also update packages.")
}

# Check OS type
os_type <- .Platform$OS.type

if (os_type == "unix") {
  # For Unix-like systems (including macOS and Linux)
  if (Sys.info()["sysname"] == "Darwin") {
    # For macOS
    xcode_installed <- system("xcode-select -p", intern = TRUE, ignore.stderr = TRUE)
    if (nchar(xcode_installed) > 0) {
      # If Xcode is installed
      remotes::install_github("QuinnAsena/multinomialTS")
    } else {
      # If Xcode is not installed
      install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.tgz", repos = NULL, type = "mac.binary")
    }
  } else {
    # For other Unix-like systems (Linux)
    remotes::install_github("QuinnAsena/multinomialTS")
  }
}

if (os_type == "windows") {
  if (dir.exists("C:\\rtools44")) {
    remotes::install_github("QuinnAsena/multinomialTS")
  } else {
    install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.zip", repos = NULL, type = "win.binary")
  }
}
