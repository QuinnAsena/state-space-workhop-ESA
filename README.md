# Welcome to the multinomialTS workshop!

There are two ways to use this workshop:
1. Using the [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/QuinnAsena/state-space-workhop-ESA/HEAD?urlpath=rstudio), which will run R studio in your browser with all the necessary packages pre-installed
   - your changes will not be saved after the binder closes!
3. Running locally by cloning or downloading this repository.

## Running locally:

If you are running locally, there are a few set-up steps necessary for everything to work smoothly:
1. **Make sure R is running version 4.4.0 or later.**
2. Update packages
3. Download or clone this repo:
   - using the green 'Code' button on the top right of the page download the .zip folder _and unzip it locally._
   - if you are familiar with git, you can clone the repo.
   - complete the following install instructions.

### Install the multinomialTS package

Open the cloned directory in file explorer / finder and double click on the .Rproj to open up R studio inside that directory. Now run the appropriate (for your OS) code in your console as described below. 

#### windows users:

If you have Rtools44 and `devtools` installed, you can build the latest version of the package directly from github using:
`devtools::install_github("https://github.com/QuinnAsena/multinomialTS")`

If you do not have Rtools and `devtools`, you can install the package with:
`install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.zip", repos = NULL, type = "win.binary")`

#### mac users:

if you have xcode-select and `devtools` installed, you can build the latest version of the package directly from github using:
`devtools::install_github("https://github.com/QuinnAsena/multinomialTS")`

If you do not have xcode-select and `devtools`, you can install the package with:
`install.packages("https://github.com/QuinnAsena/multinomialTS/releases/download/v1.0.0/multinomialTS_1.0.0.tgz", repos = NULL, type = "win.binary")`

There are a few complications with system architecture between the new and old apple chips (intel and M series). On apple it is nice and easy to download xcode select by opening a terminal and copying this code: `xcode-select --install`. Then try run: `devtools::install_github("https://github.com/QuinnAsena/multinomialTS")`


#### Alternatively

1. Open the cloned directory in file explorer / finder and double click on the .Rproj to open up R studio inside that directory.
2. Under 'files' open the `workshop_autoinstall.R` file and run the entire script.

This might not work... I wrote some script to check R version and OS and install the package accordingly, but it's janky.

## Got the package installed?

Let's check! Under 'files' in Rstudio, open up `state-space-walkthrough.qmd` in R and click the 'Render' button on the top ribbon. The first render might take a few minutes to install the necessary R packages, if everything works then a cool HTML will be rendered, by default it will pop up in Rstudio when complete.


