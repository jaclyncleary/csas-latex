## all.r
## This is the master file - it loads all packages and sources all
##  other R source code files.
## To debug in an R session, run these commands first:
## source("all.r");load.models.into.parent.env();source("custom-knitr-variables.r")

## Clean your workspace before loading
rm(list = ls(all = TRUE))

## The directory where the iscam-gui project resides
iscam.gui.dir <- "../../../iscam-gui"

## The purpose of the r-functions directory is to separate the
##  r code which is not commonly changed so that the files which are
##  can be clearly seen.
func.dir <- "r-functions"

## Need to source utilities.r before everything because it contains the functions
##  install.packages.if.needed and remove.all.except
source(file.path(func.dir, "utilities.r"))

install.packages.if.needed("devtools", "devtools", github = FALSE)
install.packages.if.needed("caTools", "caTools", github = FALSE)
install.packages.if.needed("stringi", "stringi", github = FALSE)
install.packages.if.needed("date", "date", github = FALSE)
install.packages.if.needed("xtable", "xtable", github = FALSE)
install.packages.if.needed("PBSmapping", "PBSmapping", github = FALSE)
install.packages.if.needed("PBSmodelling", "PBSmodelling", github = FALSE)
install.packages.if.needed("maps", "maps", github = FALSE)
install.packages.if.needed("coda", "coda", github = FALSE)
install.packages.if.needed("dplyr", "dplyr", github = FALSE)
install.packages.if.needed("maptools", "maptools", github = FALSE)
install.packages.if.needed("gtools", "gtools", github = FALSE)
install.packages.if.needed("knitr", "knitr", github = FALSE)

require(date)
require(r4ss)
require(xtable)
require(PBSmapping)
require(PBSmodelling)
require(maps)
require(dplyr)
require(coda)
require(gtools)
require(maptools)
require(lubridate)
require(knitr)

## Code to load the catch/TAC data, making catch figures, and making tables
##  for catch/TAC.
source(file.path(func.dir, "catches.r"))
## Code to load the models from the model directories
source(file.path(func.dir, "load-models.r"))
## Code to load the survey data, making survey figures, and making tables for survey.
source(file.path(func.dir, "survey.r"))
## Code to load data tables from the data directory
source(file.path(func.dir, "load-data.r"))
## Code to access mcmc parameters
source(file.path(func.dir, "mcmc-diagnostics.r"))
## Code to read a user file into an R list (for model setup)
## source(file.path(func.dir, "read-list.r"))

source(file.path(func.dir, "tables-parameters.r"))
source(file.path(func.dir, "tables-decisions.r"))
source(file.path(func.dir, "figures-catch.r"))
source(file.path(func.dir, "figures-age-length.r"))
source(file.path(func.dir, "figures-mcmc-diagnostics.r"))
source(file.path(func.dir, "figures-hcr.r"))
source(file.path(func.dir, "figures-indices.r"))
source(file.path(func.dir, "figures-selectivity.r"))
source(file.path(func.dir, "figures-reference-points.r"))

## Code to verify the model setup
source(file.path(func.dir, "verify.r"))
## Code to setup the model names, and start/end years for various things
##  in the models
source("model-setup.r")
## Code to setup forecast model runs
## source("forecast-catch-levels.r")
## Code to setup retro model runs.
## source("retrospective-setup.r")

## Set up variables for data tables from csv files
source("data-tables.r")
