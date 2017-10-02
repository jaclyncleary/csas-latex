###############################################################################
# 
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Quantitative Assessment Methods Section
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      Herring
# Code name:    Model.R
# Version:      2.0
# Date started: Jun 12, 2017
# Date edited:  Jun 12, 2017
# 
# Overview: 
# Make tables and figures of input data, and summarise stock assessment model
# results.
# 
# Requirements: 
# Stock assesssment model input file (e.g., *.dat), and model output files.
# 
# Notes: 
# Version 1 reads the 'old' iSCAM input and output.
# Version 2 reads the 'new' iSCAM input and output.
#
###############################################################################


########################
##### Housekeeping #####
########################

# General options
rm( list=ls( ) )      # Clear the workspace
sTime <- Sys.time( )  # Start the timer
graphics.off( )       # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, locn="https://cran.rstudio.com/" ) {
  # Reverse the list 
  rPkgs <- rev( pkgs )
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(rPkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("suppressPackageStartupMessages(library(", rPkgs[i], 
                "))", sep="")) )
  }  # End i loop over package names
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("tidyverse", "zoo", "Hmisc", "scales", "sp", "cowplot",
        "maptools", "rgdal", "rgeos", "raster", "xtable", "grid", 
        "colorRamps", "RColorBrewer", "stringr", "data.table") ) 


#################### 
##### Controls ##### 
####################     

## Location of the shared network drive (or local databases)
## TODO: Need a better way to do this (maybe like Spawn.R?). With one location
## for the access database, and another for the shapefiles.
#dirShare <- file.path( "\\\\dcbcpbsna01a", "hdata$" )
#
## Input coordinate reference system
#inCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
## Coordinate reference system (http://spatialreference.org/ref/sr-org/82/)
#outCRS <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
#    +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#
## Geographic projection
#geoProj <- "Projection: BC Albers (NAD 1983)"

# Model names (correspond to folder names)
mNames <- c( "AM2", "AM1" )


##################
#### Sources #####
##################

# # Location and name of the location database and tables
# areaLoc <- list(
#     loc=file.path(dirShare, "Stock_Assess_Database"),
#     db="HSA_BE_Locations_Main_v6.1_2015.mdb",
#     fns=list(sections="Sections", locations="Location") )
# 
# # Location(s) and names of the Sections and land shapefiles
# shapesLoc <- list(
#     locSec=file.path(dirShare, "Kristen", "Herring_Shapefiles"),
#     locLand=file.path("..", "..", "Data", "Polygons"),
#     fns=list(sections="SectionsIntegrated", land="GSHHS_h_L1_Alb") )


######################
##### Parameters #####
######################

# Possible regions by type
allRegions <- list(major=c("HG", "PRD", "CC", "SoG", "WCVI"),
                   minor=c("A27", "A2W"))

# Sensitivity runs
sens <- c("HG-natural-mortality",
          "PRD-natural-mortality",
          "CC-natural-mortality",
          "SOG-natural-mortality",
          "WCVI-natural-mortality",
          "HG-q-priors",
          "PRD-q-priors",
          "CC-q-priors",
          "SOG-q-priors",
          "WCVI-q-priors")

# Region names
allRegionNames <- list( 
    major=c("Haida Gwaii (HG)", "Prince Rupert District (PRD)", 
        "Central Coast (CC)", "Strait of Georgia (SoG)", 
        "West Coast of Vancouver Island (WCVI)"), 
    minor=c("Area 27 (A27)", "Area 2 West (A2W)") )

# Cross-walk table for SAR to region and region name
regions <- read_csv(file=
        "SAR, Region, RegionName, Major
        1, HG, Haida Gwaii, TRUE
        2, PRD, Prince Rupert District, TRUE
        3, CC, Central Coast, TRUE
        4, SoG, Strait of Georgia, TRUE
        5, WCVI, West Coast of Vancouver Island, TRUE
        6, A27, Area 27, FALSE
        7, A2W, Area 2 West, FALSE",
    col_types=cols("i", "c", "c", "l") )

# Age to highlight in figure
ageShow <- 3

# Age class of plus group for proportion-at-age
agePlusProp <- 6

# Age of recruitment
ageRec <- 2

# Number of years to calculate running mean
nRoll <- 5

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
newSurvYr <- 1988

# Intended harvest rate
intendU <- 0.2

# First year of intended harvest rate
intendUYrs <- 1983

# Figure width
figWidth <- 6

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval
ciLevel <- 0.9

# SSB quantile for production plots (grey points)
quantSSB <- 0.2

# Get ylimits (e.g., weight in kg) for the weight-at-age plot
wtRange <- c( 35, 130 ) / 1000

# 1996 fixed cutoff values (thousands of metric tonnes)
fixedCutoffs <- list( HG=10.7, PRD=12.1, CC=17.6, SoG=21.2, WCVI=18.8 )

# Proportion of B_0 for LRP
propB0 <- 0.3


#####################
##### Functions #####
#####################

# Load helper functions
source( file=file.path("Functions.R") )

# Source functions needed to read iSCAM files.
source( "read.admb.r" )


################
##### Data #####
################

# Load ADMB data from the dat file
LoadADMB <- function( SARs ) {
  # Progress message
  cat( "Loading input data... " )
  # Start a loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Get the file location: only need the first model because all input files
    # should be the same
    datLoc <- file.path( SAR, mNames[1] )
    # Look for *.dat files in the directory
    fName <- list.files( path=datLoc, pattern="*v2.dat" )
    # Read the entire file
    dat <- readLines( con=file.path(datLoc, fName) )
    # Get model dimensions index
    mIndex <- which( dat == "##### Model dimensions #####" )
    # Get number of areas
    nAreas <- scan( file=file.path(datLoc, fName), skip=mIndex, n=1, 
        quiet=TRUE )
    # Get number of groups
    nGroups <- scan( file=file.path(datLoc, fName), skip=mIndex+1, n=1, 
        quiet=TRUE )
    # Get number of sexes
    nSexes <- scan( file=file.path(datLoc, fName), skip=mIndex+2, n=1, 
        quiet=TRUE )
    # Get the first year of data
    firstYr <- scan( file=file.path(datLoc, fName), skip=mIndex+3, n=1, 
        quiet=TRUE )
    # Get the last year of data
    lastYr <- scan( file=file.path(datLoc, fName), skip=mIndex+4, n=1, 
        quiet=TRUE )
    # Get the vector of years
    yrRange <<- firstYr:lastYr
    # Get the youngest age
    youngAge <- scan( file=file.path(datLoc, fName), skip=mIndex+5, n=1, 
        quiet=TRUE )
    # Get the oldest age (i.e., plus group)
    oldAge <- scan( file=file.path(datLoc, fName), skip=mIndex+6, n=1, 
        quiet=TRUE )
    # Age range: omit below, plus group above
    ageRange <<- youngAge:oldAge
    # Get the number of gear types
    nGears <- scan( file=file.path(datLoc, fName), skip=mIndex+7, n=1,
        quiet=TRUE )
    
    # Get catch index
    cIndex <- which( dat == "##### Catch (t*10^3) #####" )
    # Get number of observations
    nObsCatch <- scan( file=file.path(datLoc, fName), skip=cIndex, n=1, 
        quiet=TRUE )
    # Get catch data
    catchRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE, 
        delim="\t", skip=cIndex+1, n_max=nObsCatch,  trim_ws=TRUE,
        col_types=paste(c("i", "i", "i", "i", "i", "i", "d"), collapse="") )
    # Update catch
    catch <- catchRaw %>%
        mutate( Region=SAR ) %>%
        rename( Year=`# Year`, Catch=Value ) %>%
        select( Region, Year, Gear, Catch )
    
    # Get spawn index
    sIndex <- which( dat == "##### Spawn (t*10^3) #####" )
    # Get number of survey types
    nSurvSpawn <- scan( file=file.path(datLoc, fName), skip=sIndex, n=1, 
        quiet=TRUE )
    # Get number of years per survey
    nSurvSpawnYrs <- scan( file=file.path(datLoc, fName), skip=sIndex+1, 
        n=nSurvSpawn, quiet=TRUE )
    # Get number of observations
    nObsSpawn <- sum( nSurvSpawnYrs )
    # Get spawn data
    spawnRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE, 
        delim="\t", skip=sIndex+3, n_max=nObsSpawn, 
        trim_ws=TRUE, 
        col_types=cols("i", "d", "i", "i", "i", "i", "d", 
            "i") )
    # Translate survey type
    survType <- read_csv( file=
            "Gear, Survey
            4, Surface
            5, Dive",
        col_types=cols("i", "c") )
    # Update spawn
    spawn <- spawnRaw %>%
        mutate( Region=SAR ) %>%
        rename( Year=`# Year` ) %>%
        left_join( y=survType, by="Gear" ) %>%
        select( Region, Year, Spawn, Survey )
    
    # Get numAged index
    nIndex <- which( dat == "##### Number-at-age #####" )
    # Get number of survey types
    nSurvNum <- scan( file=file.path(datLoc, fName), skip=nIndex, n=1, 
        quiet=TRUE )
    # Get number of years per survey
    nSurvNumYrs <- scan( file=file.path(datLoc, fName), skip=nIndex+1, 
        n=nSurvNum, quiet=TRUE )
    # Get number of observations
    nObsNum <- sum( nSurvNumYrs )
    # Get numAged data
    numAgedRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE, 
        delim="\t", skip=nIndex+6, n_max=nObsNum, 
        trim_ws=TRUE, 
        col_types=paste(rep("i", times=5+oldAge-youngAge+1), 
            collapse="") )
    # Update numAged
    numAged <- numAgedRaw %>%
        mutate( Region=SAR ) %>%
        rename( Year=`# Year` ) %>%
        select( Region, Year, Gear, as.character(ageRange) )
    
    # Get weight index
    wIndex <- which( dat == "##### Weight-at-age (kg) #####" )
    # Get number of observations
    nObsWt <- scan( file=file.path(datLoc, fName), skip=wIndex+1, n=1, 
        quiet=TRUE )
    # Get weight data
    weightRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE, 
        delim="\t", skip=wIndex+2, n_max=nObsWt, 
        trim_ws=TRUE,
        col_types=paste(c("i", "i", "i", "i", "i",
                rep("d", times=oldAge-youngAge+1)), 
            collapse="") )
    # Update weight
    weight=weightRaw %>%
        mutate( Region=SAR ) %>%
        rename( Year=`# Year` ) %>%
        select( Region, Year, as.character(ageRange) )
    
    # If it's the first region
    if( k == 1 ) {
      # Start data frames
      resCatch <- catch
      resSpawn <- spawn
      resNumAged <- numAged
      resWeight <- weight
    } else {  # End if it's the first region, otherwise
      # Append to the data frames
      resCatch <- bind_rows( resCatch, catch )
      resSpawn <- bind_rows( resSpawn, spawn )
      resNumAged <- bind_rows( resNumAged, numAged )
      resWeight <- bind_rows( resWeight, weight )
    }  # End if it's not the first region
  }  # End k loop over regions
  res <- list( catch=resCatch, spawn=resSpawn, numAged=resNumAged, 
      weight=resWeight )
  # Update the progress message
  cat( "done\n" )
  # Return the list
  return( res )
}  # End LoadADMB function

# Load ADMB data (major and minor SARs)
inputData <- LoadADMB( SARs=unlist(allRegions, use.names=FALSE) )

# Load the number of biosamples
LoadNBio <- function( SARs ) {
  # Progress message
  cat( "Loading biosamples... " )
  # Start a loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Get the number of biosamples
    nBio <- fread( input=paste("NumBiosamples", SAR, ".csv", sep=""), 
        verbose=FALSE)
    # If it's the first region
    if( k == 1 ) {
      # Start data frame
      dat <- nBio
    } else {  # End if it's the first region, otherwise
      # Append to the data frame
      dat <- bind_rows( dat, nBio )
    }  # End if it's not the first region
  }  # End k loop over regions
  # Wrangle
  res <- dat %>%
      as_tibble( ) %>%
      filter( Year %in% yrRange ) %>%
      spread( key=Region, value=Total, fill=0 )
  # Update the progress message
  cat( "done\n" )
  # Return the list
  return( res )
}  # End LoadNBio function

# Load the number of biosamples
nBio <- LoadNBio( SARs=unlist(allRegions, use.names=FALSE) )

# Arrange the ADMB output files
ArrangeOutput <- function( SARs, models ) {
  # Start a counter
  iCount <- 1
  # Message
  cat( "Arranging output files... " )
  # Loop over regions
  for( SAR in SARs ) {
    # If models is NA (not specified)
    if( is.na(models) ) {
      # Get model names (i.e., the subfolders in the region folder)
      modelsNew <- list.dirs( path=SAR, recursive=FALSE, full.names=FALSE )
    } else {  # End if models is NA, otherwise
      # Use model names provided
      modelsNew <- models
    }  # End if models is not NA
    # Loop over models
    for( model in modelsNew ) {
      # Get the path
      fn <- file.path( SAR, model )
      # Pattern for mcmc files
      mcmcPattern <- "*.csv"
      # If there is already a folder called 'mcmc': assume already processed and
      # skip to the next iteration
      if( "mcmc" %in% list.files(path=fn) )  next
      # If there are files to move, print a message
      cat( "\n\t", fn, sep="" )
      # Make a subfolder called 'mcmc'
      dir.create( path=file.path(fn, "mcmc") )
      # Get names of batch files
      batFNs <- list.files( path="Executables", pattern="*.bat", full.names=TRUE )
      # Copy bat files to 'mcmc' directory
      file.copy( from=batFNs, to=file.path(fn, "mcmc") )
      # Get names of mcmc files
      mcmcFNs <- list.files( path=fn, pattern=mcmcPattern, full.names=TRUE )
      # TODO: Warning if there are no files
      # Copy mcmc files to 'mcmc' directory
      copied <- file.copy( from=mcmcFNs, to=file.path(fn, "mcmc") )
      # If they all copied
      if( all(copied) ) {
        # Remove mcmc files
        file.remove( mcmcFNs )
      } else {  # End if copied, otherwise
        # Message
        stop( "Some", mcmcPattern, "files did not copy", call.=FALSE )
      }  # End if didn't copy
    }  # End loop over models
  }  # End loop over regions
  # Message
  cat( " done\n" )
}  # End ArrangeOutput function

# Arrange the output files (major SARs only)
ArrangeOutput( SARs=c(allRegions$major, sens), models=NA )


########################
##### Model output #####
########################

# Get model parameters
GetModelPars <- function( fn, SARs, models=mNames, probs=ciLevel ) {
  # Message
  cat( "Loading model parameters... " )
  # Get lower CI level
  lo <- (1 - probs) / 2
  # Get upper CI level
  up <- 1 - lo
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Load the file and wrangle
      raw <- fread( input=file.path(SAR, models[i], "mcmc", fn), 
              verbose=FALSE ) %>%
          as_tibble( ) %>%
          mutate( Model=model, Region=SAR ) %>%
          select( Region, Model, bo, q1, q2 ) %>%
          rename( SB0=bo )
      # Reshare to long
      rLong <- raw %>%
          gather( -Region, -Model, key="Parameter", value="Value" )
      # Perform some stats
      mp <- rLong %>%
          group_by( Region, Model, Parameter ) %>%
          summarise( 
              Lower=quantile(Value, probs=lo),
              Median=quantile(Value, probs=0.5),
              Upper=quantile(Value, probs=up) ) %>%
          ungroup( )
      # If it's the first region and model
      if( k == 1 & i == 1 ) {
        # Start a data frame
        res <- mp
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, mp )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop over regions
  # Message
  cat( "done\n" )
  # Return the data
  return( res )
}  # End GetModelPars function

# Get model parameters (major SARs only)
mPars <- GetModelPars( fn="iscam_mcmc.csv", SARs=allRegions$major )

# Assemble model parameters
GetPars <- function( fn, SARs, models=mNames, varName, probs=ciLevel ) {
  # Progress message
  cat( "Loading",  varName, "data... " )
  # Get lower CI level
  lo <- (1 - probs) / 2
  # Get upper CI level
  up <- 1 - lo
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Grab the data (transposed)
      raw <- fread( input=file.path(SAR, model, "mcmc", fn), verbose=FALSE ) %>%
          as_tibble( )
      # TODO: Perform a check to make sure recruitment is for the requested age
      # (i.e., 'ageRec').
      # Grab the years from the header names
      yrNames <- str_sub( string=names(raw), start=-4, end=-1 )
      # Calculate the median of model runs for each year
      out <- tibble( Region=SAR, Model=model, Year=as.numeric(yrNames), Parameter=varName, 
          Lower=apply(X=raw, MARGIN=2, FUN=function(x)  quantile(x, probs=lo)),
          Median=apply(X=raw, MARGIN=2, FUN=function(x)  quantile(x, probs=0.5)),
          Upper=apply(X=raw, MARGIN=2, FUN=function(x)  quantile(x, probs=up)) )
      # If it's the first region and model
      if( k == 1 & i == 1 ) {
        # Start a data frame
        res <- out
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, out )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop over regions
  # Update progress message
  cat( "done\n" )
  # Return the model output as a data frame
  return( res )
}  # End GetPars function

# Get instantaneous natural mortality (major SARs only)
natMort <- GetPars( fn="iscam_m_mcmc.csv", SARs=allRegions$major, 
    varName="Mortality" )

# Get recruitment (number in millions; major SARs only)
recruits <- GetPars( fn="iscam_rt_mcmc.csv",
    SARs=allRegions$major, varName="Recruitment" )

# Get spawning biomass (thousands of tonnes; major SARs only)
spBio <- GetPars( fn="iscam_sbt_mcmc.csv", SARs=allRegions$major, 
        varName="Abundance" ) %>%
    mutate( Survey=ifelse(Year < newSurvYr, "Surface", "Dive") )

# Assemble model values (raw)
GetVals <- function( fn, SARs, models=mNames, varName, yr ) {
  # Progress message
  cat( "Loading raw",  varName, "data... " )
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Grab the data (transposed)
      raw <- fread( input=file.path(SAR, model, "mcmc", fn), verbose=FALSE ) %>%
          as_tibble( )
      # TODO: Perform a check to make sure recruitment is for the requested age
      # (i.e., 'ageRec').
      # Grab the years from the header names
      yrNames <- str_sub( string=names(raw), start=-4, end=-1 )
      # Add the year names to columns
      colnames( raw ) <- yrNames
      # Grab the recent year data
      raw <- raw %>% 
          select( which(colnames(raw)==max(yrRange)) )
      # Calculate the median of model runs for each year
      out <- tibble( Region=SAR, Model=model, Parameter=varName, 
          Value=raw[[1]] )
      # If it's the first region and model
      if( k == 1 & i == 1 ) {
        # Start a data frame
        res <- out
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, out )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop over regions
  # Update progress message
  cat( "done\n" )
  # Return the model output as a data frame
  return( res )
}  # End GetVals function

# Get current year raw spawning biomass (thousands of tonnes, major SARs only)
spBioVals <- GetVals( fn="iscam_sbt_mcmc.csv", SARs=allRegions$major, 
    varName="Abundance", yr=2017 )

# Assemble model projections
GetProjected <- function( fn, SARs, models=mNames, probs=ciLevel ) {
  # Progress message
  cat( "Loading Projection data... " )
  # Get lower CI level
  lo <- (1 - probs) / 2
  # Get upper CI level
  up <- 1 - lo
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Generate a name for next year's projected biomass
      sbNextYr <- paste("B", max(yrRange)+1, sep="")
      # Grab the data
      raw <- fread( input=file.path(SAR, model, "mcmc", fn), verbose=FALSE ) %>%
          as_tibble( ) %>%
          rename_( "SBProj"=sbNextYr ) %>%
          mutate( Region=SAR, Model=model ) %>%
          select( Region, Model, TAC, SBProj )
      # Reshare to long
      rLong <- raw %>%
          gather( -Region, -Model, -TAC, key="Parameter", value="Value" )
      # Perform some stats
      out <- rLong %>%
          group_by( Region, Model, TAC, Parameter ) %>%
          summarise( 
              Lower=quantile(Value, probs=lo),
              Median=quantile(Value, probs=0.5),
              Upper=quantile(Value, probs=up) ) %>%
          ungroup( )
      # If it's the first region and model
      if( k==1 & i == 1 ) {
        # Start a data frame
        res <- out
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, out )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop ove regions
  # Update progress message
  cat( "done\n" )
  # Return the model output as a data frame
  return( res )
}  # End GetProjected function

# Get projected spawning biomass (major SARs only)
pPars <- GetProjected( fn="iscammcmc_proj_Gear1.csv", SARs=allRegions$major )

# Get MPD from rep file
GetMPD <- function( fn, SARs, models=mNames[1], flag, varName ) {
  # Progress message
  cat( "Loading MPD",  varName, "data... " )
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Read the file (big blob)
      obj <- read.rep( file.path(SAR, model, fn) )
      # Grab the data (transposed)
      raw <- obj[names(obj) == flag]
      # If Abundance
      if( varName == "Abundance" ) {
        # Update the year range
        yrs <- yrRange
        # Grab the object
        dat <- tibble( Abundance=raw[[1]][1:length(yrRange)] )
      }  # End if Abundance
      # If Recruitment
      if( varName == "Recruitment" ) {
        # Update the year range
        yrs <- yrRange[-c(1:min(ageRange))]
        # Grab the object
        dat <- tibble( Recruitment=raw[[1]] )
      }  # End if Recruitment
      # Calculate the median of model runs for each year
      out <- tibble( Region=SAR, Model=model, Year=yrs ) %>%
          cbind( dat ) %>%
          as_tibble( )
      # If it's the first region and model
      if( k == 1 & i == 1 ) {
        # Start a data frame
        res <- out
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, out )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop over regions
  # Update progress message
  cat( "done\n" )
  # Return the model output as a data frame
  return( res )
}  # End GetMPD function

# Get MPD (abundance)
abundMPD <- GetMPD( fn="iscam.rep", SARs=allRegions$major, flag="sbt", 
    varName="Abundance" )

# Get MPD (recruitment)
recMPD <- GetMPD( fn="iscam.rep", SARs=allRegions$major, flag="rt", 
    varName="Recruitment" )

# Get Beverton-Holt parameters
GetBHPars <- function( fn, SARs, models=mNames[1] ) {
  # Progress message
  cat( "Loading BH parameters... " )
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Loop over models
    for( i in 1:length(models) ) {
      # Get the model
      model <- models[i]
      # Read the file (big blob)
      obj <- readLines( con=file.path(SAR, model, fn) )
      # Get kappa
      kappa <- scan( file=file.path(SAR, model, fn), skip=which(obj == "kappa"), 
          n=1, quiet=TRUE )
      # Get tau
      tau <- scan( file=file.path(SAR, model, fn), skip=which(obj == "tau"), 
          n=1, quiet=TRUE )
      # Get ro
      ro <- scan( file=file.path(SAR, model, fn), skip=which(obj == "ro"), 
          n=1, quiet=TRUE )
      # Get sbo
      sbo <- scan( file=file.path(SAR, model, fn), skip=which(obj == "sbo"), 
          n=1, quiet=TRUE )
      # Calculate the median of model runs for each year
      out <- tibble( Region=SAR, Model=model, kappa=kappa, tau=tau, ro=ro, 
          sbo=sbo )
      # If it's the first region and model
      if( k == 1 & i == 1 ) {
        # Start a data frame
        res <- out
      } else {  # End if it's the first region and model, otherwise
        # Append to the data frame
        res <- bind_rows( res, out )
      }  # End if it's not the first region and model
    }  # End i loop over models
  }  # End k loop over regions
  # Update progress message
  cat( "done\n" )
  # Return the model output as a data frame
  return( res )
}  # End GetBHPars function

# Get BH parameters
bhPars <- GetBHPars( fn="iscam.rep", SARs=allRegions$major )


################
##### Main #####
################

# Format catch data for plotting
catch <- inputData$catch %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName),
        Gear=paste("Gear", Gear, sep="") ) %>%
    rename( Period=Gear )

# Format spawn index data for plotting
spawn <- inputData$spawn %>%
    mutate( Survey=factor(Survey, levels=c("Surface", "Dive"), ordered=TRUE) )%>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName) )

# Format number-at-age data for plotting
numAgedYear <- inputData$numAged %>%
    select( -Gear ) %>%
    gather( key=Age, value=Number, -Year, -Region ) %>%
    mutate( Age=as.numeric(Age),
        Age=ifelse(Age >= agePlusProp, agePlusProp, Age),
        Age=as.character(Age) ) %>%
    group_by( Region, Year, Age ) %>%
    summarise( Number=sum(Number) ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    ungroup( ) %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName) )

# Format weight-at-age data for plotting
weightLong <- inputData$weight %>%
    gather( key=Age, value=Weight, -Year, -Region )

# Calculate running mean weight-at-age by year
muWeightAge <- weightLong %>%
    group_by( Region, Age ) %>%
    mutate( muWeight=rollmean(x=Weight, k=5, align="right", na.pad=TRUE) ) %>%
    ungroup( ) %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( Age=factor(Age), 
        RegionName=factor(RegionName, levels=regions$RegionName) )

# Subset q parameters
qPars <- mPars %>%
    filter( Parameter %in% c("q1", "q2") ) %>%
    mutate( Survey=ifelse(Parameter=="q1", "Surface", "Dive") )

# Subset biomass parameters
bPars <- mPars %>%
    bind_rows( pPars ) %>%
    filter( TAC==0 | is.na(TAC) & Parameter %in% c("SB0", "SBProj") ) %>%
    mutate( Year=ifelse(Parameter=="SB0", min(yrRange)-1, max(yrRange)+1) ) %>%
    select( Region, Model, Parameter, Lower, Median, Upper, Year )

# Format current spawning biomass for plotting
spBioVals <- spBioVals %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName),
        Model=factor(Model, levels=mNames) )

# Get data for the Beverton-Holt
BevHolt <- abundMPD %>%
    full_join( recMPD, by=c("Region", "Model", "Year") ) %>%
    na.omit( ) %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName),
        Model=factor(Model, levels=mNames),
        Region=factor(Region, levels=regions$Region) )
    
# Get data for the effective harvest rate
harvRate <- catch %>%
    group_by( Region, Year ) %>%
    summarise( Catch=SumNA(Catch) ) %>%
    ungroup( ) %>%
    full_join( y=spBio, by=c("Region", "Year") ) %>%
    mutate( LowerHR=Catch/(Lower+Catch), MedianHR=Catch/(Median+Catch),
        UpperHR=Catch/(Upper+Catch) ) %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName),
        Model=factor(Model, levels=mNames),
        Region=factor(Region, levels=regions$Region))

# Get maximum Abundance by Region and Model
maxAbund <- BevHolt %>%
    group_by( Region, Model ) %>%
    summarise( MaxAbund=MaxNA(Abundance) ) %>%
    ungroup( )

# Generate predicted line for Beverton-Holt relationship
predBH <- bhPars %>%
    full_join( y=maxAbund, by=c("Region", "Model") ) %>%
    group_by( Region, Model, kappa, tau, ro, sbo ) %>%
    expand( Abundance=seq(from=0, to=max(MaxAbund, sbo), length.out=100) ) %>%
    mutate( Recruitment=kappa * ro * Abundance / 
            (sbo + (kappa-1) * Abundance) * exp(-0.5 * tau^2) ) %>%
    ungroup( ) %>%
    left_join( y=regions, by="Region" ) %>%
    mutate( RegionName=factor(RegionName, levels=regions$RegionName),
        Model=factor(Model, levels=mNames),
        Region=factor(Region, levels=regions$Region) )


###################
##### Figures #####
###################

# Message
cat( "Printing figures... " )

# Plot catch by year and gear type (i.e., period)
PlotCatch <- function( SARs, dat ){ 
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get region name
    SAR <- SARs[k]
    # Subset the data
    datSub <- dat %>%
        filter( Region==SARs[k] )  # Why does 'Region==SAR' no longer work?
    # The plot
    catchGearPlot <- ggplot( data=datSub, aes(x=Year, y=Catch) ) + 
        geom_bar( stat="identity", position="stack", aes(fill=Period) ) +
        labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        scale_fill_grey( start=0, end=0.8 ) + 
        myTheme +
        theme( legend.position=c(0.99, 0.98), legend.justification=c(1, 1) ) +
        ggsave( filename=file.path(SAR, "CatchGear.png"), width=figWidth, 
            height=figWidth*0.67 )
  }  # End k loop over regions
  # Get major stocks
  datMajor <- dat %>%
      filter( Region%in%allRegions$major )
  # Plot all the regions together
  catchPlotAll <- ggplot( data=datMajor, aes(x=Year, y=Catch) ) + 
      geom_bar( stat="identity", position="stack" ) +
      labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
      scale_y_continuous( labels=comma ) +
      facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
      myTheme +
      ggsave( filename=file.path("Catch.png"), width=figWidth, height=figWidth )
}  # End PlotCatch function

# Plot catch (major and minor SARs)
PlotCatch( SARs=regions$Region, dat=catch )

# Plot total spawn index by year
PlotSpawn <- function( SARs, dat ){ 
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get region name
    SAR <- SARs[k]
    # Subset the data
    datSub <- dat %>%
        filter( Region==SARs[k] )  # Why does 'Region==SAR' no longer work?
    # The plot
    spawnIndexPlot <- ggplot( data=datSub, aes(x=Year, y=Spawn) ) +
        geom_point( aes(shape=Survey) ) + 
        geom_line( aes(group=Survey) ) +
        labs( x=NULL, y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        scale_shape_manual( values=c(1, 2) ) +
        expand_limits( y=0 ) +
        myTheme +
        theme( legend.position=c(0.01, 0.98), legend.justification=c(0, 1) ) +
        ggsave( filename=file.path(SAR, "SpawnIndex.png"), width=figWidth, 
            height=figWidth*0.67 )
  }  # End k loop over regions
  # Get major stocks
  datMajor <- dat %>%
      filter( Region%in%allRegions$major )
  # The plot
  spawnIndexPlotAll <- ggplot( data=datMajor, aes(x=Year, y=Spawn) ) +
      geom_point( aes(shape=Survey) ) + 
      geom_line( aes(group=Survey) ) +
      labs( x=NULL, y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
      scale_y_continuous( labels=comma ) +
      scale_shape_manual( values=c(1, 2) ) +
      expand_limits( y=0 ) +
      facet_wrap( ~ RegionName, ncol=2, dir="v", scales="free_y" ) +
      myTheme +
      theme( legend.position="top" ) +
      ggsave( filename=file.path("SpawnIndex.png"), width=figWidth, 
          height=figWidth+0.5 )
}  # End PlotSpawn function

# Plot spawn index (major and minor SARs)
PlotSpawn( SARs=unlist(allRegions, use.names=FALSE), dat=spawn )

# Plot proportion-at-age
PlotAge <- function( SARs, dat ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get region name
    SAR <- SARs[k]
    # Subset the data
    datSub <- dat %>%
        filter( Region==SARs[k] )  # Why does 'Region==SAR' no longer work?
    # The plot
    propPlot <- ggplot( data=datSub, aes(x=Year, y=Proportion, group=Age) ) +
        geom_bar( aes(fill=Age), stat="identity" ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        expand_limits( y=0 ) +
        labs( y="Proportion-at-age" ) +
        scale_fill_brewer( type="qual", palette="Set1", 
            guide=guide_legend(nrow=1) ) +
        myTheme +
        theme( legend.position="top" ) +
        ggsave( filename=file.path(SAR, "ProportionAge.png"), width=figWidth, 
            height=figWidth*0.67 )
  }  # End k loop over regions
  # Get major stocks
  datMajor <- dat %>%
      filter( Region%in%allRegions$major )
  # The plot
  propPlot <- ggplot( data=datMajor, aes(x=Year, y=Proportion, group=Age) ) +
      geom_bar( aes(fill=Age), stat="identity" ) +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
      expand_limits( y=0 ) +
      labs( y="Proportion-at-age" ) +
      scale_fill_brewer( type="qual", palette="Set1", 
          guide=guide_legend(nrow=1) ) +
      facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
      myTheme +
      theme( legend.position="top" ) +
      ggsave( filename=file.path("ProportionAge.png"), width=figWidth, 
          height=figWidth )
}  # End PlotAge function

# Plot proportion-at-age (major and minor SARs)
PlotAge( SARs=unlist(allRegions, use.names=FALSE), dat=numAgedYear )

# Plot weight-at-age by year
PlotWeight <- function( SARs, dat ){ 
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get region name
    SAR <- SARs[k]
    # Subset the data
    datSub <- dat %>%
        filter( Region==SARs[k] )  # Why does 'Region==SAR' no longer work?
    # The plot
    weightAgePlot <- ggplot( data=datSub ) + 
        geom_line( aes(x=Year, y=muWeight, group=Age) ) +
        geom_point( data=filter(.data=datSub, Age == ageShow), 
            aes(x=Year, y=Weight), shape=1, size=1 ) +
        geom_line( data=filter(.data=datSub, Age == ageShow), 
            aes(x=Year, y=muWeight), size=1 ) +
        labs( y="Weight-at-age (kg)" ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        coord_cartesian( ylim=wtRange ) +
        myTheme +
        ggsave( filename=file.path(SAR, "WeightAge.png"), width=figWidth, 
            height=figWidth*0.67 )
  }  # End k loop over regions
  # Get major stocks
  datMajor <- dat %>%
      filter( Region%in%allRegions$major )
  # Plot all the regions together
  weightAgePlotAll <- ggplot( data=datMajor ) + 
      geom_line( aes(x=Year, y=muWeight, group=Age) ) +
      geom_point( data=filter(.data=datMajor, Age == ageShow), 
          aes(x=Year, y=Weight), shape=1, size=1 ) +
      geom_line( data=filter(.data=datMajor, Age == ageShow), 
          aes(x=Year, y=muWeight), size=1 ) +
      labs( y="Weight-at-age (kg)" ) +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
      coord_cartesian( ylim=wtRange ) +
      facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
      myTheme +
      ggsave( filename=file.path("WeightAge.png"), width=figWidth, 
          height=figWidth )
}  # End PlotWeight function

# Plot weight-at-age (major and minor SARs)
PlotWeight( SARs=unlist(allRegions, use.names=FALSE), dat=muWeightAge )

# Plot number aged by year
PlotNumber <- function( SARs, dat ){ 
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get region name
    SAR <- SARs[k]
    # Subset the data
    datSub <- dat %>%
        filter( Region==SARs[k] )  # Why does 'Region==SAR' no longer work?
    # The plot
    numberAgedPlot <- ggplot( data=datSub, aes(x=Year, y=Number, group=Year) ) + 
        geom_bar( stat="identity" ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) + 
        myTheme +
        ggsave( filename=file.path(SAR, "NumberAged.png"), width=figWidth, 
            height=figWidth*0.67 )
  }  # End k loop over regions
  # Get major stocks
  datMajor <- dat %>%
      filter( Region%in%allRegions$major )
  # Plot all the regions together
  numberAgePlotAll <- ggplot( data=datMajor, aes(x=Year, y=Number, group=Year) ) + 
      geom_bar( stat="identity" ) +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
      scale_y_continuous( labels=comma ) + 
      facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
      myTheme +
      ggsave( filename=file.path("NumberAged.png"), width=figWidth, 
          height=figWidth )
}  # End PlotNumber function

# Plot number aged (major and minor SARs)
PlotNumber( SARs=unlist(allRegions, use.names=FALSE), dat=numAgedYear )

# Story-board plot: 4 panels (abundance, recruitment, M, and SSB with catch)
PlotStoryboard <- function( SARs, models, si, qp, rec, M, SSB, C, bp, mName ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Loop over models
    for( i in 1:length(models) ) {
      # Get region name
      SAR <- SARs[k]
      # Get model name
      model <- models[i]
      # Default point size
      pSize <- 0.75
      # Defauly line thickness
      lSize <- 0.5
      # Update the theme for the storyboard
      myTheme2 <- myTheme + 
          theme( plot.margin=unit(c(0.25, 0.75, 0, 0), "lines"),
              text=element_text(size=8) )
      # Need to extend the year range by 1
      rangeX <- c( min(yrRange-1), max(yrRange)+1 )
      # Data wrangling: abundance
      siSub <- si %>%
          mutate( Survey=as.character(Survey) ) %>%
          filter( Region==SARs[k] ) %>%
          full_join( y=filter(qp, Model==model, Region==SARs[k]), 
              by=c("Region", "Survey") ) %>%
          mutate( Abundance=Spawn/Median )
      # Data wrangling: recruitment
      recSub <- rec %>%
          filter( Model==model, Region==SARs[k] ) 
      # Data wrangling: natural mortality
      MSub <- M %>%
          filter( Model==model, Region==SARs[k] )
      # Data wrangling: spawning biomass
      SSBSub <- SSB %>%
          filter( Model==model, Region==SARs[k] )
      # Data wrangling: catch
      CSub <- C %>%
          filter( Catch > 0, Region==SARs[k] )
      # Data wrangling: SB_0
      SB0Sub <- bp %>%
          filter( Model==model, Parameter=="SB0", Region==SARs[k] )
      # Data wrangling: SB_projected
      SBProjSub <- bp %>%
          filter( Model==model, Parameter=="SBProj", Region==SARs[k] )
      # Plot a: abundance
      plotA <- ggplot( data=siSub, aes(x=Year, y=Abundance) ) +
          geom_point( aes(shape=Survey), size=pSize ) + 
          geom_line( data=SSBSub, aes(x=Year, y=Median, group=Survey), 
              size=lSize ) +
          expand_limits( x=rangeX, y=0 ) +
          labs( x=NULL, 
              y=expression(paste("Scaled abundance (t"%*%10^3,")", sep="")) ) +
          scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
          scale_shape_manual( values=c(2, 1) ) +
          #      scale_linetype_manual( values=c(2, 1) ) +
          guides( shape=FALSE, linetype=FALSE ) +
          annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, 
              hjust=-0.1, size=2.5 ) +
          myTheme2 +
          theme( axis.text.x=element_blank() )
      # Plot b: recruitment
      plotB <- ggplot( data=recSub, aes(x=Year, y=Median) ) +
          geom_point( size=pSize ) +
          geom_errorbar( aes(ymin=Lower, ymax=Upper), size=lSize/2, width=0 ) +
          expand_limits( x=rangeX, y=0 ) +
          labs( y=paste("Number of age-", ageRec, " recruits (millions)", 
                  sep="") ) +
          scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
          scale_y_continuous( labels=comma ) +
          annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, 
              hjust=-0.1, size=2.5 ) +
          myTheme2
      # Plot c: natural mortality
      plotC <- ggplot( data=MSub, aes(x=Year, y=Median) ) + 
          geom_ribbon( aes(ymin=Lower, ymax=Upper), alpha=0.5 ) +
          geom_line( size=lSize ) + 
          expand_limits( x=rangeX, y=0 ) +
          labs( x=NULL, y="Instantaneous natural mortality" ) +
          scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
          annotate( geom="text", x=-Inf, y=Inf, label="(c)", vjust=1.3, 
              hjust=-0.1, size=2.5 ) +
          myTheme2 +
          theme( axis.text.x=element_blank() )
      # Plot d: spawning biomass and catch
      plotD <- ggplot( data=SSBSub, aes(x=Year, y=Median) ) +
          geom_bar( data=CSub, aes(x=Year, y=Catch), stat="identity", 
              width=lSize, fill="black" ) +
          geom_ribbon( aes(ymin=Lower, ymax=Upper), alpha=0.5 ) +
          geom_line( size=lSize ) + 
#          # SB_0
#          geom_point( data=SB0Sub, aes(x=Year, y=Median), size=pSize ) +
#          geom_errorbar( data=SB0Sub, aes(ymin=Lower, ymax=Upper), size=lSize/2, 
#              width=0 ) +
          # SB_Proj
          geom_point( data=SBProjSub, aes(x=Year, y=Median), size=pSize ) +
          geom_errorbar( data=SBProjSub, aes(ymin=Lower, ymax=Upper), 
              size=lSize/2, width=0 ) +
          expand_limits( x=rangeX, y=0 ) +
          labs( y=expression(paste("Spawning biomass (t"%*%10^3,")", 
                      sep="")) ) +
          scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
          annotate( geom="text", x=-Inf, y=Inf, label="(d)", vjust=1.3, 
              hjust=-0.1, size=2.5 ) +
          myTheme2
      # Make a title
      # pTitle <- ggdraw( ) + 
      #   draw_label( label=paste(unique(areas$RegionName), " (", model, ")", 
      #                           sep=""), size=8 )
      # Combine the plots
      storyboard <- plot_grid( plotA, plotC, plotB, plotD, align="v", ncol=2, 
              nrow=2, rel_heights=c(1.0, 1.1) ) +
          # Add the title and write to disc
#          pStory <- plot_grid( pTitle, storyboard, ncol=1, 
#              rel_heights=c(0.1, 1.8) ) +
          ggsave( filename=file.path(SAR, paste("Storyboard", model, ".png", 
                      sep="")), width=figWidth, height=0.7*figWidth )
    }  # End i loop over models
  }  # End k loop over regions
}  # End PlotStoryboard function

# Make the storyboard (major SARs only)
PlotStoryboard( SARs=allRegions$major, models=mNames, si=spawn, qp=qPars, 
    rec=recruits, M=natMort, SSB=spBio, C=catch, bp=bPars )

# Plot distribution of spawning biomass in current year, and the LRP
PlotCurrentSSB <- function( SARs, models, SSB, SB0, probs=ciLevel ) {
  # Get lower CI level
  lo <- (1 - probs) / 2
  # Get upper CI level
  up <- 1 - lo
  # Update SSB
  SSB <- SSB %>%
      mutate( RegionName=factor(RegionName, levels=regions$RegionName), 
          Model=factor(Model, levels=mNames) )
  # Calculate LRP
  LRP <- SB0 %>%
      mutate( Lower=Lower*propB0, Median=Median*propB0, Upper=Upper*propB0 ) %>%
      left_join( y=regions, by="Region" ) %>%
      mutate( RegionName=factor(RegionName, levels=regions$RegionName),
          Model=factor(Model, levels=mNames) )
  # SSB quantiles
  quantSSB <- SSB %>%
      group_by( RegionName, Region, Model ) %>%
      summarise( Lower=quantile(Value, probs=lo),
          Median=quantile(Value, probs=0.5),
          Upper=quantile(Value, probs=up) ) %>%
      ungroup( )
  # The plot
  plotSSB <- ggplot( data=SSB ) + 
      geom_density( aes(x=Value), fill="grey" ) + 
#      geom_vline( data=LRP, aes(xintercept=Lower), colour="red", 
#          linetype="dashed" ) +
      geom_vline( data=LRP, aes(xintercept=Median), colour="red" ) +
      geom_rect( data=LRP, aes(xmin=Lower, xmax=Upper, ymin=-Inf, ymax=Inf),
          colour="transparent", fill="red", alpha=0.3 ) + 
#      geom_vline( data=LRP, aes(xintercept=Upper), colour="red", 
#          linetype="dashed" ) +
      geom_vline( data=quantSSB, aes(xintercept=Lower), linetype="dashed" ) +
      geom_vline( data=quantSSB, aes(xintercept=Median) ) +
      geom_vline( data=quantSSB, aes(xintercept=Upper), linetype="dashed" ) +
      facet_wrap( Model ~ Region, scales="free", ncol=2, dir="v", 
          labeller=label_wrap_gen(multi_line=FALSE) ) +
      labs( x=expression(paste("SB"[2017]," (t"%*%10^3, ")")), y="Density" ) +
      myTheme +
      ggsave( filename=file.path("CurrentSSB.png"), width=figWidth, 
          height=figWidth )
}  # End PlotCurrentSSB function

# Show current SSB
PlotCurrentSSB( SARs=allRegions$major, models=mNames, SSB=spBioVals, 
    SB0=filter(bPars, Parameter=="SB0") )

# Plot effective harvest rate
PlotHarvestRate <- function( hr, SARs, models ) {
  # Filter for desired regions and areas
  hrSub <- hr %>%
      filter( Region %in% SARs, Model %in% models )
  # The plot
  plotSSB <- ggplot( data=hrSub, aes(x=Year) ) + 
      geom_ribbon( aes(ymin=LowerHR, ymax=UpperHR), fill="grey" ) +
      geom_line( aes(y=MedianHR) ) +
      annotate( geom="segment", x=intendUYrs, y=intendU, xend=max(yrRange), 
          yend=intendU, linetype="dashed" ) +
      facet_grid( Region ~ Model ) +
      labs( y="Effective harvest rate" ) +
      expand_limits( y=c(0, 1) ) +
      myTheme +
      ggsave( filename=file.path("HarvestRate.png"), width=figWidth, 
          height=figWidth )
}  # End PlotHarvestRate function

# Plot harvest rate
PlotHarvestRate( hr=harvRate, SARs=allRegions$major, models=mNames )

# Plot Beverton-Holt stock-recruitment relationship
PlotBevertonHolt <- function( bh, bhPred, SARs, models ) {
  # Filter for desired regions and areas
  bhSub <- bh %>%
      filter( Region %in% SARs )
  # Filter for desired regions and areas
  bhPredSub <- bhPred %>%
      filter( Region %in% SARs ) %>%
      # TODO: Confirm if we should use this, or the "uncorrected" ro for points
      mutate( ro2=ro*exp(-0.5*tau^2) )
  # The plot
  plotBH <- ggplot( data=bhSub, aes(x=Abundance, y=Recruitment) ) + 
      geom_point( aes(colour=Year==max(yrRange)) ) +
      geom_point( data=bhPredSub, aes(x=sbo, y=ro), shape=17 ) +
      geom_line( data=bhPredSub ) + 
      scale_colour_grey( start=0.5, end=0 ) +
      facet_wrap( ~ RegionName, ncol=2, scales="free", dir="v" ) +
      labs( x=expression(paste("Spawning biomass (t"%*%10^3, ")")), 
          y=paste("Number of age-", ageRec, " recruits (millions)", sep="") ) +
      scale_y_continuous( label=comma ) +
      expand_limits( x=0, y=0 ) +
      guides( colour=FALSE ) +
      myTheme +
      ggsave( filename=file.path("BevertonHolt.png"), width=figWidth, 
          height=figWidth )
}  # End PlotBevertonHolt function

# Plot Beverton-Holt
PlotBevertonHolt( bh=filter(BevHolt, Model==mNames[1]), SARs=allRegions$major,
    bhPred=filter(predBH, Model==mNames[1]) )

# Message
cat( "done\n" )


###################
##### xTables #####
###################

# Format q values table
xQPars <- qPars %>%
    mutate( Lower=format(Lower, digits=4, nsmall=4),
        Median=format(Median, digits=4, nsmall=4),
        Upper=format(Upper, digits=4, nsmall=4),
        Parameter=ifelse(Survey=="Surface", "$q_{1}$", "$q_{2}$") ) %>%
    select( Region, Model, Survey, Parameter, Lower, Median, Upper ) %>%
    xtable( )

# Write q parameters to disc
print( x=xQPars, file="QPars.tex", include.rownames=FALSE, booktabs=TRUE, 
    only.contents=TRUE, NA.string=NA, sanitize.text.function=identity, 
    sanitize.colnames.function=identity )

# Format biomass values table
xBPars <- bPars %>%
    mutate( Lower=format(Lower, digits=2, nsmall=2, big.mark=","),
        Median=format(Median, digits=2, nsmall=2, big.mark=","),
        Upper=format(Upper, digits=2, nsmall=2, big.mark=","),
        Parameter=ifelse(Parameter=="SB0", "$\\mli{SB}_{0}$", 
            paste("$\\mli{SB}_{", Year, "}$", sep="")) ) %>%
    select( Region, Model, Parameter, Lower, Median, Upper ) %>%
    xtable( )

# Write biomass parameters to disc
print( x=xBPars, file="BPars.tex",include.rownames=FALSE, booktabs=TRUE, 
    only.contents=TRUE, NA.string=NA, sanitize.text.function=identity, 
    sanitize.colnames.function=identity )

# Print catch
PrintCatch <- function( SARs, dat ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Format catch
    xCatch <- dat %>%
        filter( Region==SAR ) %>%
        select( -Region ) %>%
        mutate( Gear=paste("Gear", Gear, sep="") ) %>%
        complete( Year=yrRange, Gear=c("Gear1", "Gear2", "Gear3"), 
            fill=list(Catch=0) ) %>%
        spread( Gear, Catch ) %>%
        xtable( digits=c(0, 0, 3, 3, 3) )
    # Write catch to longtable
    WriteLongTable( dat=xCatch, fn=file.path(SAR, "Catch.tex") )
    # Column names for catch
    myNames <- paste( names(xCatch), collapse=" & " )
  }  # End k loop over regions
  # Return column names
  return( myNames )
}  # End PrintCatch function

# Print catch and get column names
namesCatch <- PrintCatch( SARs=unlist(allRegions, use.names=FALSE), 
    dat=inputData$catch )

# Print spawn index
PrintSpawn <- function( SARs, dat ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Format spawn
    xSpawn <- dat %>%
        filter( Region==SAR ) %>%
        select( -Region ) %>%
        rename( `Spawn index (t$\\times 10^{3}$)`=Spawn) %>%
        xtable( digits=c(0, 0, 3, 0) )
    # Write spawn to longtable
    WriteLongTable( dat=xSpawn, fn=file.path(SAR, "Spawn.tex") )
    # Column names for spawn
    myNames <- paste( names(xSpawn), collapse=" & " )
  }  # End k loop over regions
  # Return column names
  return( myNames )
}  # End PrintSpawn function

# Print spawn and get column names
namesSpawn <- PrintSpawn( SARs=unlist(allRegions, use.names=FALSE), 
    dat=inputData$spawn )

# Print number-at-age
PrintAge <- function( SARs, dat ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Format number-at-age
    xNumAged <- dat %>%
        filter( Region==SAR ) %>%
        select( -Region ) %>%
        xtable( )
    # Write number-at-age to longtable
    WriteLongTable( dat=xNumAged, fn=file.path(SAR, "NumAged.tex") )
    # Column names for number-at-age
    myNames <- paste( names(xNumAged), collapse=" & " )
  }  # End k loop over regions
  # Return column names
  return( myNames )
}  # End PrintAge function

# Print number-at-age and get column names
namesNumAged <- PrintAge( SARs=unlist(allRegions, use.names=FALSE), 
    dat=inputData$numAged )

# Print weight-at-age
PrintWeight <- function( SARs, dat ) {
  # Loop over regions
  for( k in 1:length(SARs) ) {
    # Get the region
    SAR <- SARs[k]
    # Format weight-at-age
    xWeight <- dat %>%
        filter( Region==SAR ) %>%
        select( -Region ) %>%
        xtable( digits=c(0, 0, rep(3, times=max(ageRange)-min(ageRange)+1)) )
    # Write weight-at-age to longtable
    WriteLongTable( dat=xWeight, fn=file.path(SAR, "Weight.tex") )
    # Column names for weight-at-age
    myNames <- paste( names(xWeight), collapse=" & " )
  }  # End k loop over regions
  # Return column names
  return( myNames )
}  # End PrintWeight function

# Print weight-at-age and get column names
namesWeight <- PrintWeight( SARs=unlist(allRegions, use.names=FALSE), 
    dat=inputData$weight )

# Print the number of biosamples
PrintNBio <- function( dat ) {
  # Format number of biosamples
  xNBio <- dat %>%
      xtable( digits=c(0, 0, rep(0, times=length(unlist(allRegions)))) )
  # Write number of biosamples to longtable
  WriteLongTable( dat=xNBio, fn="NBio.tex" )
  # Column names for number of biosamples
  myNames <- paste( names(xNBio), collapse=" & " )
  # Return column names
  return( myNames )
}  # End PrintNBio function

# Print number of biosamples and get column names
namesNBio <- PrintNBio( dat=nBio )


#################
##### LaTeX #####
#################

# Number of years in the time series
nYrs <- length( yrRange )

# Current season code
thisSeason <- paste( yrRange[nYrs-1], yrRange[nYrs], sep="/" )

# Formatted year ranges for q1 (surface) and q2 (dive)
qYrs <- list(
    q1=paste(range(yrRange[yrRange<newSurvYr]), collapse="--"),
    q2=paste(range(yrRange[yrRange>=newSurvYr]), collapse="--") )

# Spawn in current year
finalYrSpawn <- inputData$spawn %>%
    # Get spawn in final year of timeseries
    filter( Year == max(yrRange) ) %>%
    # Convert to tonnes, and format nicely
    mutate( Spawn=format(Spawn*1000, big.mark=",", digits=0, scientific=FALSE) )


############################
##### Production (ARK) #####
############################

# Source Rob's production analysis
source( file="herrSP.r" )


##################
##### Tables #####
##################



##################
##### Output #####
##################

# # Save the workspace image 
save.image( file="Image.RData" ) 


############### 
##### End ##### 
############### 

# Print end of file message and elapsed time
cat( "End of file Model.R: ", sep="" ) ;  print( Sys.time( ) - sTime )
