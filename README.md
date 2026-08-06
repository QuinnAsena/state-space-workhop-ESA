# EDIT

A newer version of this workshop is at: https://github.com/QuinnAsena/multinomialTS-workshop :)


# Welcome to the multinomialTS workshop!

There are two ways to use this workshop:
1. Using the [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/QuinnAsena/state-space-workhop-ESA/HEAD?urlpath=rstudio), which will run RStudio in your browser with all the necessary packages pre-installed.
   - Note: Binder can take 5–10 minutes to launch on a cold start — this is normal!
   - Your changes will not be saved after the Binder session closes.
2. Running locally by cloning or downloading this repository.

## Running locally

If you are running locally, there are a few set-up steps necessary for everything to work smoothly:
1. **Make sure R is version 4.4.0 or later.**
2. Update your packages.
3. Download or clone this repo:
   - Using the green 'Code' button on the top right of the page, download the .zip folder _and unzip it locally._
   - If you are familiar with git, you can clone the repo.
4. Complete the install instructions below.

### Install packages (automated — recommended)

Open the cloned directory in file explorer / finder and double-click the `.Rproj` file to open RStudio inside that directory. Then run the following in your R console:

```r
source("workshop_autoinstall.R")
```

This script detects your OS, checks for the necessary build tools, and installs `multinomialTS` and all supporting packages automatically.

### Install the multinomialTS package (manual)

If you prefer to install manually, open the `.Rproj` file to launch RStudio in the project directory, then follow the instructions for your OS below.

#### Windows users

If you have Rtools44 and `devtools` installed, build the latest version from GitHub:
```r
devtools::install_github("https://github.com/QuinnAsena/multinomialTS")
```

If you do not have Rtools44 and `devtools`, install the pre-built binary:
```r
install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.zip", repos = NULL, type = "win.binary")
```

#### macOS users

Install `xcode-select` by opening a terminal and running:
```
xcode-select --install
```
Then install the package from GitHub:
```r
devtools::install_github("https://github.com/QuinnAsena/multinomialTS")
```

If you do not have `xcode-select` or `devtools`, use the automated script above (`workshop_autoinstall.R`), which will fall back to a pre-built binary.

## Got the package installed?

Let's check! In the RStudio console, run:
```r
library(multinomialTS)
```
If that loads without error, you're good to go. Then open `state-space-walkthrough.qmd` and click the 'Render' button. The first render may take a few minutes to install the remaining R packages; if everything works, a HTML document will open in RStudio.


