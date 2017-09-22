## -----------------------------------------------------------------------------
## Set verbosity for this project (R code) and SS shell calls
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.05, 0.95)

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
start.yr <- 1951
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}
## Start year for the fishery age comps
start.yr.age.comps <- 1951
if(verbose){
  cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
}
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2016
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2016
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Directories and names of stocks
## -----------------------------------------------------------------------------
stock.dir <- list()
stock.name <- list()
stock.dir[[1]] <- "HG"
stock.name[[1]] <- "Haida Gwaii"
stock.dir[[2]] <- "PRD"
stock.name[[2]] <- "Pr. Rupert"
stock.dir[[3]] <- "CC"
stock.name[[3]] <- "Central Coast"
stock.dir[[4]] <- "SOG"
stock.name[[4]] <- "SOG"
stock.dir[[5]] <- "WCVI"
stock.name[[5]] <- "WCVI"

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
base.model.name <- lapply(1:length(stock.name),
                          function(x){
                            paste("Reference model", stock.name[[x]])})

base.model.dir.name <- lapply(1:length(stock.dir),
                              function(x){
                                file.path(stock.dir[[x]], "AM2")})

lapply(1:length(base.model.dir.name),
       function(x){
         verify.models(model.dir,
                       base.model.dir.name[[x]],
                       base.model.name[[x]])})

if(verbose){
  lapply(1:length(base.model.dir.name),
         function(x){
           cat0("Base model directory name for ",
                stock.name[[x]],
                ":\n  ",
                base.model.dir.name[[x]])
           cat0("Base model pretty name for ",
                stock.name[[x]],
                ":\n  ",
                base.model.name[[x]])})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## This is a list of the q-prior sensitivities, one for each stock
## -----------------------------------------------------------------------------
sens.model.dir.name.1 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1")})

sens.model.name.1 <- "AM1"

lapply(1:length(sens.model.dir.name.1),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.1[[x]],
                       sens.model.name.1)})

if(verbose){
  lapply(1:length(sens.model.dir.name.1),
         function(x){
           print.model.message(sens.model.dir.name.1[[x]],
                               sens.model.name.1,
                               1,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Vector of directory names for all models referenced above
## -----------------------------------------------------------------------------
## ALL models must be in this list!
## Each model directory listed here will have an RData file in it,
##  or one will be created depending on what is found in the directory.
##  i.e. mcmc, retrospective, or forecast directories.
model.dir.names <- c(base.model.dir.name,
                     unlist(sens.model.dir.name.1))

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.models <<- lapply(base.model.dir.name,
                         function(x){
                           load.models(model.dir, x)})
  sens.models.1 <<- lapply(sens.model.dir.name.1,
                           function(x){
                             load.models(model.dir, x)})
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

  ## Base models
  invisible(lapply(1:length(base.model.dir.name),
                   function(x){
                     ## Determine which stock is being loaded
                     bm <- base.model.dir.name[[x]]
                     if(length(grep("HG", bm))){
                       which.stock <- 1
                     }else if(length(grep("PRD", bm))){
                       which.stock <- 2
                     }else if(length(grep("CC", bm))){
                       which.stock <- 3
                     }else if(length(grep("SOG", bm))){
                       which.stock <- 4
                     }else if(length(grep("WCVI", bm))){
                       which.stock <- 5
                     }else{
                       which.stock <- 0
                     }
                     create.rdata.file(model.name = bm,
                                       ovwrt.rdata = ovwrt.base,
                                       load.proj = TRUE,
                                       low = confidence.vals[1],
                                       high = confidence.vals[2],
                                       burnin = 0,
                                       which.stock = which.stock,
                                       which.model = 2,
                                       verbose = ss.verbose)}))

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the lapply below to work right
  sens.model.names.list <- c(unlist(sens.model.dir.name.1))

  ## Sensitivity models
  invisible(lapply(1:length(sens.model.names.list),
                   function(x){
                     ## Determine which stock is being loaded
                     sm <- sens.model.names.list[[x]]
                     if(length(grep("HG", sm))){
                       which.stock <- 1
                     }else if(length(grep("PRD", sm))){
                       which.stock <- 2
                     }else if(length(grep("CC", sm))){
                       which.stock <- 3
                     }else if(length(grep("SOG", sm))){
                       which.stock <- 4
                     }else if(length(grep("WCVI", sm))){
                       which.stock <- 5
                     }else{
                       which.stock <- 0
                     }
                     create.rdata.file(
                       model.name = sm,
                       ovwrt.rdata = ovwrt.sens,
                       load.proj = TRUE,
                       low = confidence.vals[1],
                       high = confidence.vals[2],
                       burnin = 0,
                       which.stock = which.stock,
                       which.model = 1,
                       verbose = ss.verbose)}))

}
