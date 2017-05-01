## -----------------------------------------------------------------------------
## Set verbosity for this project (R code) and SS shell calls
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.025, 0.975)

## -----------------------------------------------------------------------------
## iscam files with names that don't change depending on model
rep.file <- "iscam.rep"
par.file <- "iscam.par"
mcmc.file <- "iscam_mcmc.csv"
mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
mcmc.recr.file <- "iscam_rt_mcmc.csv"
mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
mpd.proj.file <- "iscammpd_proj_Gear1.csv"
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- as.numeric(substr(Sys.Date(), 1, 4))
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## -----------------------------------------------------------------------------
model.dir <- file.path("..", "..", "models")
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
exe.file.name <- "iscam.exe"
if(verbose) cat0("iscam executable file: \n  ", exe.file.name)
starter.file.name <- "iscam.dat"
if(verbose) cat0("iscam starter file: \n  ", starter.file.name)

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Start year for the models
start.yr <- 1996
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}
## Start year for the fishery age comps
start.yr.age.comps <- 1996
if(verbose){
  cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
}
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2014
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2014
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Base model name and directory
## -----------------------------------------------------------------------------
base.model.dir.name <- "01-base"
base.model.name <- "Reference model"
verify.models(model.dir, base.model.dir.name, base.model.name)
if(verbose){
  cat0("Base model directory name: \n  ", base.model.dir.name)
  cat0("Base model pretty name: \n  ", base.model.name)
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.model.dir.names.1 <- c("02-sigma-0.1",
                            "03-estimated-total-variance")
sens.model.names.1 <- c("Sigma = 0.1",
                        "Estimated total variance")
verify.models(model.dir, sens.model.dir.names.1, sens.model.names.1)
if(verbose){
  print.model.message(sens.model.dir.names.1,
                      sens.model.names.1,
                      1,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## -----------------------------------------------------------------------------
sens.model.dir.names.2 <- c("04-tau-1.0",
                            "05-tau-0.6")
sens.model.names.2 <- c("Tau = 1.0",
                        "Tau = 0.6")
verify.models(model.dir, sens.model.dir.names.2, sens.model.names.2)
if(verbose){
  print.model.message(sens.model.dir.names.2,
                      sens.model.names.2,
                      2,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## -----------------------------------------------------------------------------
sens.model.dir.names.3 <- "06-low-steepness"
sens.model.names.3 <- "Low steepness"
verify.models(model.dir, sens.model.dir.names.3, sens.model.names.3)
if(verbose){
  print.model.message(sens.model.dir.names.3,
                      sens.model.names.3,
                      3,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## -----------------------------------------------------------------------------
sens.model.dir.names.4 <- c("07-m-0.2-sd-0.05",
                            "08-m-0.2-sd-0.25")
sens.model.names.4 <- c("M = 0.2, SD = 0.05",
                        "M = 0.2, SD = 0.25")
verify.models(model.dir, sens.model.dir.names.4, sens.model.names.4)
if(verbose){
  print.model.message(sens.model.dir.names.4,
                      sens.model.names.4,
                      4,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 5
## -----------------------------------------------------------------------------
sens.model.dir.names.5 <- "09-m-0.3-sd-0.2"
sens.model.names.5 <- "M = 0.3, SD = 0.2"
verify.models(model.dir, sens.model.dir.names.5, sens.model.names.5)
if(verbose){
  print.model.message(sens.model.dir.names.5,
                      sens.model.names.5,
                      5,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 6
## -----------------------------------------------------------------------------
sens.model.dir.names.6 <- "10-qm0-qsd1"
sens.model.names.6 <- "Q priors, mean = ln(1.5)"
verify.models(model.dir, sens.model.dir.names.6, sens.model.names.6)
if(verbose){
  print.model.message(sens.model.dir.names.6,
                      sens.model.names.6,
                      6,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 7
## -----------------------------------------------------------------------------
sens.model.dir.names.7 <- "11-qm05-qsd15"
sens.model.names.7 <- "Q priors, mean = ln(0.5), SD = 1.5"
verify.models(model.dir, sens.model.dir.names.7, sens.model.names.7)
if(verbose){
  print.model.message(sens.model.dir.names.7,
                      sens.model.names.7,
                      7,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Sensitivity models group 8
## -----------------------------------------------------------------------------
sens.model.dir.names.8 <- "12-fix-trawl-sel-amat"
sens.model.names.8 <- "Fix trawl sel to maturity ogive"
verify.models(model.dir, sens.model.dir.names.8, sens.model.names.8)
if(verbose){
  print.model.message(sens.model.dir.names.8,
                      sens.model.names.8,
                      8,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Vector of directory names for all models referenced above
## -----------------------------------------------------------------------------
## ALL models must be in this list!
## Each model directory listed here will have an RData file in it,
##  or one will be created depending on what is found in the directory.
##  i.e. mcmc, retrospective, or forecast directories.
model.dir.names <- c(base.model.dir.name,
                     sens.model.dir.names.1,
                     sens.model.dir.names.2,
                     sens.model.dir.names.3,
                     sens.model.dir.names.4,
                     sens.model.dir.names.5,
                     sens.model.dir.names.6,
                     sens.model.dir.names.7,
                     sens.model.dir.names.8)

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model <<- load.models(model.dir, base.model.dir.name)
  sens.models.1 <<- load.models(model.dir, sens.model.dir.names.1)
  sens.models.2 <<- load.models(model.dir, sens.model.dir.names.2)
  sens.models.3 <<- load.models(model.dir, sens.model.dir.names.3)
  sens.models.4 <<- load.models(model.dir, sens.model.dir.names.4)
  sens.models.5 <<- load.models(model.dir, sens.model.dir.names.5)
  sens.models.6 <<- load.models(model.dir, sens.model.dir.names.6)
  sens.models.7 <<- load.models(model.dir, sens.model.dir.names.7)
  sens.models.8 <<- load.models(model.dir, sens.model.dir.names.8)
}

build <- function(ovwrt.base = FALSE,
                  ovwrt.sens = FALSE){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.
  ##
  ## ovwrt.base - overwrite the RData file for the base model?
  ## ovwrt.sens - overwrite the RData files for the sensitivity models?

  ## Base model
  create.rdata.file(model.name = base.model.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the FOR loop below to work right
  mnv <- c(unlist(sens.model.dir.names.1),
           unlist(sens.model.dir.names.2),
           unlist(sens.model.dir.names.3),
           unlist(sens.model.dir.names.4),
           unlist(sens.model.dir.names.5),
           unlist(sens.model.dir.names.6),
           unlist(sens.model.dir.names.7),
           unlist(sens.model.dir.names.8))

  ## Remove base model from the bridge/sensitivity list
  if(length(grep(base.model.dir.name, mnv)) > 0){
    mnv <- mnv[-(grep(base.model.dir.name, mnv))]
  }
  model.names.list <- as.list(unique(mnv))

  ## Sensitivity models
  for(model.nm in model.names.list){
    create.rdata.file(
      model.name = model.nm,
      ovwrt.rdata = ovwrt.sens,
      load.proj = TRUE,
      low = confidence.vals[1],
      high = confidence.vals[2],
      verbose = ss.verbose)
  }
}
