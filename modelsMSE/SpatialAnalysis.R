###############################################################################
# 
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Offshore Assessment, Aquatic Resources, Research, and Assessment
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      Herring
# Code name:    SpatialAnalysis.R
# Version:      1.0
# Date started: Mar 21, 2017
# Date edited:  Jun 02, 2017
# 
# Overview: 
# Plot some spawn and catch data on a smaller spatial scale (i.e., statistical
# area, section, location code) as a proxy for having management and stock
# assessment on a smaller spatial scale, which is not possible with the current
# framework.
# 
# Requirements: 
# The saved image from 'Summary.R', as well as raw spawn data in the same
# directory (which is currently written there by 'SpawnIndex.R'). The external
# program ImageMagick (https://www.imagemagick.org/script/index.php) is also 
# required to make the *.gif.
# 
# Notes: 
# Averages and other statistics are for the locations that have values. For
# example, 'LayersMean' in 'siYr' is the average number of egg layers for
# all the locations that have spawn reported. That is to say, it excludes
# locations with no spawn (which have zero layers).
#
# References:
# 
#
###############################################################################


########################
##### Housekeeping #####
########################

# General options
sTime2 <- Sys.time( )  # Start the timer


################
##### Data #####
################

# Extract q parameters
qParsSub <- qPars %>%
    filter( Region == region, Model %in% mNames ) %>%
    rename( qLower=Lower, qMedian=Median, qUpper=Upper ) %>%
    select( Model, qLower, qMedian, qUpper, Survey )

# If old directory exists
if( region %in% list.files() ) {
  # Remove the old directory
  unlink( region, recursive=TRUE )
  # Warning: remove previous summary output
  warning( "Removed existing directory '", region, "'", call.=FALSE )
  # Create the main directory for output
  dir.create( region )
} else {  # End if directory exists, otherwise
  # Create the main directory for output
  dir.create( region )
}  # End if directory doesn't exists

# Get data from the data summary
LoadSavedObjects <- function( loc ) {
  # Load the saved object
  load( file=file.path("Data", paste("Image.", loc, ".RData", sep="")) )
  # Confirm the region (from the saved data -- should match!)
  if( regName != spRegions[reg] )
    stop( "Region mismatch -- check saved image", call.=FALSE )
  # Return objects to the main environment
  newSurvYr <<- newSurvYr  # Year in which survey type changed
  yrRange <<- yrRange  # Range of years to consider
  inCRS <<- inCRS  # Coordinate reference system (input)
  outCRS <<- outCRS  # Coordinate reference system (output)
  shapes <<- shapes  # Shapefiles etc
  figWidth <<- figWidth*1.2  # Figure size
  catch <<- catch  # Catch data
  UpdateCatchData <<- UpdateCatchData  # Function to update catch data
  inclTestCatch <<- inclTestCatch  # Include catch from test fishery
  geoProj <<- geoProj  # Text for maps: projection
  areas <<- areas  # Spatial info
  bio <<- bio  # Biological data
  spawnRaw <<- spawnRaw  # Spawn data
  ciLevel <<- ciLevel  # Confidence interval
  ageRange <<- ageRange  # Ages
  qYrs <<- qYrs  # Survey years (q1 and q2)
  smLine <<- smLine  # Smoothing function for line
}  # End LoadSavedObjects function

# Load saved data (directly!)
LoadSavedObjects( loc=region )

# Function to load transect spatial info
LoadTransectXY <- function( loc ) {
  # Load the data and wrangle
  dat <- read_excel( path=loc, sheet=1 ) %>%
      rename( LocationCode=LOC_CODE ) %>%
      mutate( LocationCode=as.integer(LocationCode) ) %>%
      group_by( LocationCode ) %>%
      summarise( 
          Longitude=MeanNA(c(StartLong, MidLong, EndLong)),
          Latitude=MeanNA(c(StartLat, MidLat, EndLat)) ) %>%
      ungroup( ) %>%
      filter( !is.na(Longitude), !is.na(Latitude), LocationCode!=0 )
  # Grab the spatial info (X and Y)
  locSP <- dat %>%
      transmute( X=Longitude, Y=Latitude )
  # Put X and Y into a spatial points object
  locPts <- SpatialPointsDataFrame( coords=locSP, 
      data=data.frame(LocationCode=dat$LocationCode), proj4string=CRS(inCRS) )
  # Convert X and Y from WGS to Albers
  locPtsAlb <- spTransform( x=locPts, CRSobj=CRS(outCRS) )
  # Extract spatial info
  dfAlb <- as_tibble( locPtsAlb ) %>%
      rename( Eastings=X, Northings=Y )
  # Return the data
  return( dfAlb )
}  # End LoadTransectXY function

# Load 'auxiliary' dive transect spatial data
transectXY <- LoadTransectXY( loc=diveFN )

# Get a short list of areas: sections and groups
areasSm <- areas %>%
    select( Region, StatArea, Group, Section ) %>%
    distinct( )

# Update shapefiles: allowed spatial units
if( spUnitName %in% c("Region", "StatArea", "Group", "Section") ) {
  # If spatial unit is region
  if( spUnitName == "Region" ) {
    # Make new data frames
    shapes$spUnitDF <- shapes$regDF %>% rename_( SpUnit=spUnitName ) 
    shapes$spUnitCentDF <- shapes$regCentDF %>% 
        rename_( SpUnit=spUnitName ) %>%
        filter( SpUnit == region )
  }  # End if statistical areas
  # If spatial unit is statistical areas
  if( spUnitName == "StatArea" ) {
    # Make new data frames
    shapes$spUnitDF <- shapes$saDF %>% rename_( SpUnit=spUnitName ) 
    shapes$spUnitCentDF <- shapes$saCentDF %>% rename_( SpUnit=spUnitName )
  }  # End if statistical areas
  # If spatial unit is groups
  if( spUnitName == "Group" ) {
    # Make new data frames
    shapes$spUnitDF <- shapes$grpDF %>% rename_( SpUnit=spUnitName ) 
    shapes$spUnitCentDF <- shapes$grpCentDF %>% rename_( SpUnit=spUnitName )
  }  # End if groups
  # If spatial unit is sections
  if( spUnitName == "Section" ) {
    # Make new data frames
    shapes$spUnitDF <- shapes$secDF %>% rename_( SpUnit=spUnitName ) 
    shapes$spUnitCentDF <- shapes$secCentDF %>% rename_( SpUnit=spUnitName )
  }  # End if groups
} else {  # End if spatial units are defined, otherwise
  # Error
  stop( "Variable spUnitName = ", spUnitName, " not defined", call.=FALSE )
}  # End if spatial units are not defined

# Get spawn index data
GetSI <- function( allSI, loc, XY ) { 
  # Get a subset and wrangle
  raw <- allSI %>%
      replace_na( list(SurfSI=0, MacroSI=0, UnderSI=0, Group="Other") ) %>%
      mutate( Decade=GetDecade(Year), Area=Length*Width, 
          Survey=ifelse(Year < newSurvYr, "Surface", "Dive"),
          YrsSurv=ifelse(Year < newSurvYr, length(yrRange[yrRange<newSurvYr]),
              length(yrRange[yrRange>=newSurvYr])) ) %>%
      rowwise( ) %>%
      mutate( LyrsMean=MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs)),
          SITotal=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
      ungroup( ) %>%
      group_by( Decade ) %>%
      mutate( YrsDecade=length(unique(Year)) ) %>%
      ungroup( )
  # Clip the extent
  df <- ClipExtent( dat=raw, spObj=shapes$regSPDF, bufDist=maxBuff, 
      silent=TRUE )
  # Subset data with 'good' X and Y
  dfNotNA <- df %>%
      filter( !is.na(Eastings) & !is.na(Northings) )
  # Subset data with 'bad' X or Y, and try to fill in using transect X and Y
  dfNA <- df %>%
      filter( is.na(Eastings) | is.na(Northings) ) %>%
      select( -Eastings, -Northings ) %>%
      left_join( y=XY, by="LocationCode" )
  # Re-combine the two subsets
  df2 <- bind_rows( dfNotNA, dfNA )
  # Clip the extent (again)
  res <- ClipExtent( dat=df2, spObj=shapes$regSPDF, bufDist=maxBuff )
  # Stop if we're missing rows
  if( nrow(raw) != nrow(res) )  stop( "Missing rows!", call.=FALSE )
  # Return the spatial data
  return( res )
}  # End GetSI function

# Get spawn index
siAll <- GetSI( allSI=spawnRaw, loc=region, XY=transectXY )


################ 
##### Main ##### 
################     

# Rename variables to set spatial resolution
areas <- rename_( .data=areas, SpUnit=spUnitName )
bio <- rename_( .data=bio, SpUnit=spUnitName )
catch <- rename_( .data=catch, SpUnit=spUnitName )
siAll <- rename_ (.data=siAll, SpUnit=spUnitName )

# Aggregate catch by year and spatial unit
catchYrSp <- catch %>%
    group_by( Year, SpUnit ) %>%
    summarise( Catch=SumNA(Catch) ) %>%
    ungroup( ) %>%
    complete( Year, SpUnit ) %>%
    arrange( Year, SpUnit ) 

# Update years (use the same year to compare timing among years)
year( siAll$Start ) <- 0000
year( siAll$End ) <- 0000

# Make it long (for plots)
siAllLong <- siAll %>%
    gather( 'Start', 'End', key="Timing", value="Date" )

# Aggregate spawn index by year and spatial unit
siYrSp <- siAll %>%
    group_by( Year, SpUnit ) %>%
    summarise( 
        NumLocs=n_distinct(LocationCode),
        LengthTotal=SumNA(Length),
        WidthMean=MeanNA(Width),
        AreaTotal=SumNA(Area),
        LayersMean=MeanNA(LyrsMean),
        SITotal=SumNA(SITotal),
        DateFirst=MinNA(Start, End),
        DateLast=MaxNA(Start, End),
        DateDiff=DateLast-DateFirst,
        Survey=unique(Survey) ) %>%
    group_by( SpUnit ) %>%
    mutate( NConsec=CountConsecutive(Year) ) %>%
    ungroup( ) %>%
    complete( Year, SpUnit ) %>%
    arrange( Year, SpUnit ) %>%
    left_join( y=qParsSub, by="Survey" ) %>%
    mutate( BiomassLower=SITotal/qUpper, BiomassMedian=SITotal/qMedian,
        BiomassUpper=SITotal/qLower, 
        Survey=factor(Survey, levels=c("Surface", "Dive")) )

## Convert spawn index to scaled biomass
#siScaledYrSp <- siYrSp %>%
#    group_by( Year ) %>%
#    mutate( Proportion=SITotal/SumNA(SITotal) ) %>%
#    ungroup( )

# Combine catch with spawn index by year and spatial unit
allYrSp <- full_join( x=catchYrSp, y=siYrSp, by=c("Year", "SpUnit") ) %>%
    arrange( Year, SpUnit ) %>%
    replace_na( replace=list(NConsec=-1) ) %>%
    mutate( Survey=factor(Survey, levels=c("Surface", "Dive")) )

# Count the number of fish aged by year (and as a proportion) by seine gear:
# use the 'SampWt' column to fix unrepresentative sampling if identified
npAgedYear <- bio %>%
    filter( GearCode == 29 ) %>%
    select( Year, SpUnit, Age, SampWt ) %>%
    na.omit( ) %>%
    group_by( Year, SpUnit, Age ) %>%
    summarise( Number=SumNA(SampWt) ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    ungroup( ) %>%
    arrange( Year, SpUnit, Age )

# Calculate proportion at age by year and spatial unit
propAge <- npAgedYear %>%
    mutate( AgeClass=ifelse(Age >= 6, '6+', Age) ) %>%
    group_by( Year, SpUnit, AgeClass ) %>%
    summarise( Proportion=sum(Proportion) ) %>%
    ungroup( ) %>%
    complete( Year, SpUnit, AgeClass, fill=list(Proportion=0) ) %>%
    group_by( SpUnit, Year ) %>%
    mutate( AllZero=all(Proportion==0) ) %>%
    ungroup( ) %>%
    filter( !AllZero ) %>%
    group_by( SpUnit, AgeClass ) %>%
    mutate( GroupID=ConsecutiveGroup(Year) ) %>%
    ungroup( ) %>%
    rename( Age=AgeClass )

# Calculate number aged by year and spatial unit
numAge <- npAgedYear %>%
    group_by( Year, SpUnit ) %>%
    summarise( nAged=sum(Number) ) %>%
    ungroup( )

# Merge proportion at age with spawn index: spawn index is now repeated!
allYrSpPA <- full_join( x=allYrSp, y=propAge, by=c("Year", "SpUnit") )

# Get detailed layers infomation: spatial unit and type
lyrsYrUnit <- siAll %>% 
    select( Year, SpUnit, SurfLyrs, MacroLyrs, UnderLyrs ) %>%
    gather( SurfLyrs, MacroLyrs, UnderLyrs, key="Type", value="Layers" ) %>%
    group_by( Year, SpUnit ) %>%
    summarise( LayersMean=MeanNA(Layers) ) %>%
    ungroup( )  %>%
    complete( Year, SpUnit )

# Get spawn index metrics by year and location
siYearLoc <- siAll %>%
    group_by( Year, Survey, Decade, SpUnit, LocationCode ) %>%
    summarise( YrsDecade=unique(YrsDecade), YrsSurv=unique(YrsSurv), 
        Eastings=unique(Eastings), Northings=unique(Northings),
        SITotal=SumNA(SITotal) ) %>%
    ungroup( )

# Get spawn index metrics by decade and location code
siDecadeLoc <- siAll %>%
    group_by( Decade, SpUnit, LocationCode ) %>%
    summarise( Persistence=length(unique(Year))/unique(YrsDecade),
        Eastings=unique(Eastings), Northings=unique(Northings),
        SITotalMean=MeanNA(SITotal) ) %>%
    ungroup()

# Get spawn index metrics by survey
siSurvey <- siAll %>%
    group_by( Survey, SpUnit, LocationCode ) %>%
    summarise( Persistence=length(unique(Year))/unique(YrsSurv),
        Eastings=unique(Eastings), Northings=unique(Northings) ) %>%
    ungroup( )

#siSummary <- siAll %>%
#    filter( Survey=="Dive" ) %>%
#    group_by( Year ) %>%
#    summarise( 
#        SA6=sum(SITotal[SpUnit%in%c(6)]),
#        SA7=sum(SITotal[SpUnit%in%c(7)]),
#        SA8=sum(SITotal[SpUnit%in%c(8)]),
#        SA67=sum(SITotal[SpUnit%in%c(6, 7)]),
#        SA678=sum(SITotal[SpUnit%in%c(6, 7, 8)]) )

# Calculate weighted mean spawn index (spatial) by year
siWeightedYear <- siAll %>%
    group_by( Year ) %>%
    summarise( Eastings=WtMeanNA(x=Eastings, w=SITotal),
        Northings=WtMeanNA(x=Northings, w=SITotal),
        SITotal=SumNA(SITotal), Area=SumNA(Length)*MeanNA(Width) ) %>%
    ungroup( ) %>%
    mutate( EastingsPrev=lag(Eastings, n=1), NorthingsPrev=lag(Northings, n=1) )


###################
##### Figures #####
###################

# Plot the BC coast and regions
if( reg == 1 )  
  BCMap <- ggplot( data=shapes$landAllCropDF, aes(x=Eastings, y=Northings) ) +
      geom_polygon( data=shapes$landAllCropDF, aes(group=group), 
          fill="lightgrey" ) +
      geom_point( data=shapes$extAllDF, colour="transparent" ) +
      geom_path( data=shapes$regAllDF, aes(group=Region), size=0.75, 
          colour="black" ) + 
      geom_label( data=shapes$regCentDF, alpha=0.5, aes(label=Region) ) +
      annotate( geom="text", x=1100000, y=800000, label="British\nColumbia",
          size=5 ) +
      annotate( geom="text", x=650000, y=550000, label="Pacific\nOcean", 
          size=5 ) +
      coord_equal( ) +
      labs( x="Eastings (km)", y="Northings (km)", caption=geoProj ) +
      scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
      scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
      myTheme +
      ggsave( filename=file.path("BC.png"), width=figWidth, 
          height=7/shapes$xyAllRatio )

# Function to plot the data
PlotPairs <- function( dat, sub ) {
  # Start the pdf
  pdf( file=file.path(region, paste("Pairs", sub, ".pdf", sep="")), 
      width=figWidth, height=figWidth )
  # The plot: pairs
  plotPairs <- ggpairs( data=dat ) +
      myTheme +
      labs( title=sub ) +
      theme( text=element_text(size=8), panel.spacing=unit(0, "lines") )
  # Print the plot
  print( plotPairs )
  # Turn the device off
  dev.off( )
}  # End PlotPairs function

## Plot pairs: all
#PlotPairs( dat=allYrSp %>% 
#        select( NumLocs, LengthTotal, WidthMean, AreaTotal, LayersMean, SITotal, 
#            DateDiff, Catch ) %>%
#        na.omit( ), sub="All" )
#
## Plot pairs: dive
#PlotPairs( dat=allYrSp %>% 
#        filter( Year >= newSurvYr ) %>%
#        select( NumLocs, LengthTotal, WidthMean, AreaTotal, LayersMean, SITotal, 
#            DateDiff, Catch ) %>%
#        na.omit( ), sub="Dive" )

# Plot spawn index, proportion aged, and number aged
PlotSIEtAl <- function( dat1, dat2, dat3, siThresh=siThreshold, 
    nYrs=nYrsConsec ) {
  # Determine the number of pages
  uPages <- unique( dat1$SpUnit )
  # Range in years
  yrRange <- range( dat1$Year, na.rm=TRUE )
  # Range in spawn index
  siRange <- range( dat1$SITotal, na.rm=TRUE )
  # Range in numbers aged
  naRange <- range( dat3$nAged, na.rm=TRUE )
  # Loop over pages 
  for( i in 1:length(uPages) ) {
    # Subset the data
    df1 <- filter( .data=dat1, SpUnit==uPages[i] )
    df2 <- filter( .data=dat2, SpUnit==uPages[i] )
    df3 <- filter( .data=dat3, SpUnit==uPages[i] )
    # Plot spawn index (1)
    siPlot <- ggplot( dat=df1, aes(x=Year, y=SITotal) ) +
        geom_path( ) +
        geom_point( size=2.5 ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        expand_limits( x=yrRange, y=siRange ) +
        labs( x=NULL ) +
        myTheme +
        facet_wrap( ~ SpUnit ) +
        theme( axis.text.x=element_blank(), text=element_text(size=24) )
    # Add the reference line if supplied
    if( !is.na(siThresh) )  siPlot <- siPlot + 
          geom_hline( yintercept=siThresh, linetype="dashed", size=0.5 )
    # Plot proportion at age (2)
    paPlot <- ggplot( data=df2, aes(x=Year, y=Proportion) ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        expand_limits( x=yrRange, y=c(0, 1) ) +
        geom_bar( aes(fill=Age), stat="identity", width=1 ) +
        labs( x=NULL ) +
        scale_fill_brewer( type="qual", palette="Set1", 
            guide=guide_legend(nrow=1) ) +
        myTheme +
        theme( axis.text.x=element_blank(), legend.position="top", 
            text=element_text(size=24) )
    # Plot number aged (3)
    naPlot <- ggplot( data=df3, aes(x=Year, y=nAged) ) +
        geom_bar( stat="identity", width=1 ) + 
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        expand_limits( x=yrRange, y=naRange ) +
        myTheme +
        theme( text=element_text(size=24) )
    # Combine the plots
    siEtAlPlots <- plot_grid( siPlot, paPlot, naPlot, align="v", ncol=1, 
            rel_heights=c(1, 1, 1) ) +
        ggsave( file=file.path(region, 
                paste("AgeComposition", i, ".png", sep="")), 
            height=figWidth*1.5, width=figWidth )
  }  # End i loop over pages
}  # End PlotSIEtAl function

# Plot the proportions at age
PlotSIEtAl( dat1=allYrSp, dat2=propAge, dat3=numAge )

# Make proportion-at-age bubble plots
PlotPropAgeBubble <- ggplot( data=npAgedYear, aes(x=Year, y=Age) ) +
    geom_point( aes(size=Proportion) ) + 
    scale_size_area( max_size=3 ) + 
    scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
    facet_wrap( ~ SpUnit, ncol=1 ) +
    myTheme + 
    theme( legend.position="top" ) +
    ggsave( filename=file.path(region, "AgeBubbles.png"), width=figWidth, 
        height=min(9, n_distinct(npAgedYear$SpUnit)*2+1) )

# Make a default map for the area
plotMap <- ggplot( data=shapes$landCropDF, aes(x=Eastings, y=Northings) ) +
    geom_polygon( data=shapes$landCropDF, aes(group=group), fill="lightgrey" ) +
    geom_path( data=shapes$regDF, aes(group=Region), size=0.75, 
        colour="black" ) +
    geom_path( data=shapes$spUnitDF, aes(group=SpUnit), size=0.25, 
        colour="black" ) +
    coord_equal( ) +
    labs( x="Eastings (km)", y="Northings (km)", caption=geoProj ) +
    scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
    scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
    myTheme + 
    theme( text=element_text(size=18) )

# Make the map: show spatial units
mapSpUnits <- plotMap +
    geom_label_repel( data=shapes$spUnitCentDF, alpha=0.75,
        aes(label=SpUnit), size=6, point.padding=unit(0, "lines") ) +
    ggsave( filename=file.path(region, "MapSpUnits.png"), width=figWidth, 
        height=min(8, figWidth/shapes$xyRatio) )

# Plot locations and persistence: continuous
mapPersistence <- plotMap +
    geom_point( data=siSurvey, aes(size=Persistence), alpha=0.5 ) +
    facet_wrap( ~ Survey, ncol=1 ) +
    theme( legend.position=c(0.99, 0.99), legend.justification=c(1, 1),
        legend.box="horizontal" ) +
    ggsave( filename=file.path(region, "MapPersistence.png"), width=figWidth, 
        height=min(8, 6/shapes$xyRatio)*2 )  

# Show spawn index locations by year
PlotLocationsYear <- function( dat, yVar, yLegend ) {
  # Update the temporary directory: within the region folder
  tDirReg <- file.path("GIFs", region )
  # If old directory exists
  if( file.exists(tDirReg) ) {
    # Remove the old directory
    unlink( tDirReg, recursive=TRUE )
    # Create the main directory for output
    dir.create( tDirReg, recursive=TRUE )
  } else {  # End if directory exists, otherwise
    # Create the main directory for output
    dir.create( tDirReg, recursive=TRUE )
  }  # End if directory doesn't exist
  # Get the number of plots
  uPages <- unique( dat$Year )
  # Start a progress message
  cat( "Plotting ", length(uPages), " pages: ", sep="" )
  # Get indices to print
  pIndices <- round( x=quantile(x=1:length(uPages), probs=1:9/10) )
  # Loop over pages/years
  for( i in 1:length(uPages) ) { 
    # Get the index (up to 9999)
    iLong <- formatC( uPages[i], width=4, flag="0" )
    # The plot
    layersPlot <- plotMap +
        facet_wrap_paginate( ~ Year, ncol=1, nrow=1, page=i ) +
        geom_point( data=dat, aes_string(colour=yVar), size=7 ) +
        scale_colour_distiller( type="seq", palette="Spectral", labels=comma ) +
        labs( colour=yLegend ) +
        theme( legend.position=c(0.99, 0.99), legend.justification=c(1, 1),
            legend.box="horizontal", legend.text.align=1 ) +
        ggsave( file=file.path(tDirReg, 
                paste("LocationsYear", yVar, iLong, ".png", sep="")), 
            width=figWidth, height=figWidth/shapes$xyRatio+0.25 )
    # Update progress message
    if( i %in% pIndices )  cat( i, ", ", sep="" )
  }  # End i loop over decades
#  # Update progressEnd message
#  cat( "done; making GIF...", sep="" )
#  # Get the list of plot names
#  pNames <- list.files( tDirReg )
#  # Read the plots as images
#  images <- lapply( file.path(tDirReg, pNames), image_read )
#  # Animate the images
#  anim <- image_animate( image=image_join(images), fps=1 )
#  # Save images as a gif
#  image_write( image=anim, path=file.path("GIFs", 
#          paste("LocationsYear", yVar, region, ".gif", sep="")), quality=100 )
  # End message
  cat( " done\n", sep="" )
}  # End PlotLocationsYear

# Show spawn locations (this takes a few minutes!)
if( makeGIF )  PlotLocationsYear( dat=siYearLoc, yVar="SITotal", 
      yLegend="Spawn\nindex (t)" )

# Show spawn index locations by decade
PlotLocationsDecade <- function( dat, yVar ) {
  # Get the number of plots
  uPages <- unique( dat$Decade )
  # Loop over pages
  for( i in 1:length(uPages) ) { 
    # The plot
    layersPlot <- plotMap +
        facet_wrap_paginate( ~ Decade, ncol=1, nrow=1, page=i ) +
        geom_point( data=dat, aes_string(size="Persistence", colour=yVar),
            alpha=0.75) +
        scale_colour_distiller( type="seq", palette="Spectral", labels=comma ) + 
        theme( legend.position=c(0.99, 0.99), legend.justification=c(1, 1),
            legend.box="horizontal" ) +
        ggsave( file=file.path(region, 
                paste("LocationsDecade", yVar, i, ".png", sep="")), 
            width=figWidth, height=figWidth/shapes$xyRatio+0.25 )
  }  # End i loop over decades  
}  # End PlotLocationsDecade

# Show spawn locations
PlotLocationsDecade( dat=siDecadeLoc, yVar="SITotalMean" )

# Plot spawn index vs spawn metric(s)
ScatterSI <- function( df, yVar, siThresh=siThreshold, nYrs=nYrsConsec ) {
  # Determine the number of pages
  uPages <- unique( df$SpUnit )
  # Range in spawn index
  siRange <- range( 0, df$SITotal, na.rm=TRUE )
  # Range in y
  yRange <- range( 0, df[[yVar]], na.rm=TRUE )
  # Range in years
  yrRange <- range( df$Year, na.rm=TRUE )
  # Loop over pages
  for( i in 1:length(uPages) ) {
    # Subset the data
    dat <- df %>% 
        filter( SpUnit==uPages[i] )
    # Extra filters for age classes
    if( yVar == "Proportion" ) 
      dat <- dat %>% filter( !is.na(Age) )
    # Make the plot
    plt <- ggplot( data=dat, aes_string(x="SITotal", y=yVar) ) +
        geom_point( size=2.5 ) +
        scale_x_continuous( labels=comma ) +
        scale_y_continuous( labels=comma ) +
        scale_colour_distiller( type="seq", palette="Spectral" ) +
        expand_limits( x=siRange, y=yRange ) +
        myTheme +
        guides( shape=FALSE ) +
        theme( text=element_text(size=24), legend.position="top" )
    # Add the reference line if supplied
    if( !is.na(siThresh) )  plt <- plt + 
          geom_vline( xintercept=siThresh, linetype="dashed", size=0.5 )
    # If proportions at age
    if( yVar == "Proportion" ) {
      plt <- plt + 
          facet_wrap( ~ Age, ncol=2, labeller=label_both ) +
          labs( title=uPages[i] ) +
          theme( plot.title=element_text(hjust=0.5) )# +
      #geom_smooth( )
    } else {  # End if proportion at age, otherwise business as usual
      plt <- plt +
          facet_wrap( ~ SpUnit )
    }  # End if business as usual
    # Print the plot
    plt <- plt + 
        ggsave( filename=file.path(region, 
                paste("Scatter", yVar, i, ".png", sep="")), 
            width=figWidth, 
            height=ifelse(yVar=="Proportion", 1.3*figWidth, 0.7*figWidth) )
  }  # End i loop over pages
}  # End ScatterSI function

# Plot spawn index: scatterplots (yearly summary)
ScatterSI( df=allYrSp, yVar="NumLocs" )
ScatterSI( df=allYrSp, yVar="LengthTotal" )
ScatterSI( df=allYrSp, yVar="WidthMean" )
ScatterSI( df=allYrSp, yVar="AreaTotal" )
ScatterSI( df=allYrSp, yVar="LayersMean" )
ScatterSI( df=allYrSp, yVar="DateDiff" )
ScatterSI( df=allYrSp, yVar="Catch" )
ScatterSI( df=allYrSpPA, yVar="Proportion" )

# Plot spawn index timeseries
TimeseriesSI <- function( df, yVar, siThresh=siThreshold, nYrs=nYrsConsec ) {
  # Determine the number of pages
  uPages <- unique( df$SpUnit )
  # Range in spawn index
  siRange <- range( 0, df$SITotal, na.rm=TRUE )
  # Range in y
  yRange <- range( 0, df[[yVar]], na.rm=TRUE )
  # Range in years
  yrRange <- range( df$Year, na.rm=TRUE )
  # Loop over pages
  for( i in 1:length(uPages) ) {
    # Subset the data
    dat <- df %>% 
        filter( SpUnit == uPages[i] )
    # Spawn index time series
    tsSI <- ggplot( data=dat, aes(x=Year, y=SITotal) ) +
        geom_path( ) +
        geom_point( size=2.5 ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        expand_limits( x=yrRange, y=siRange ) +
        labs( x=NULL ) +
        myTheme +
        facet_wrap( ~ SpUnit, nrow=1, ncol=1 ) +
        theme( axis.text.x=element_blank(), text=element_text(size=28) )
    # Add the reference line if supplied
    if( !is.na(siThresh) )  tsSI <- tsSI + 
          geom_hline( yintercept=siThresh, linetype="dashed", size=0.5 )
    # The second timeseries
    ts2 <- ggplot( data=dat, aes_string(x="Year", y=yVar) ) +
        geom_path( ) +
        geom_point( size=2.5 ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        scale_y_continuous( labels=comma ) +
        expand_limits( x=yrRange, y=yRange ) +
        myTheme +
        theme( text=element_text(size=28) )
    # Combine the two plots
    tsPlots <- plot_grid( tsSI, ts2, align="v", ncol=1, rel_heights=1 ) +
        ggsave( file=file.path(region, 
                paste("Timeseries", yVar, i, ".png", sep="")), 
            width=figWidth, height=figWidth )
  }  # End loop over pages
}  # End TimeseriesSI function

# Plot spawn index: timeseries (yearly summary)
TimeseriesSI( df=allYrSp, yVar="NumLocs" )
TimeseriesSI( df=allYrSp, yVar="LengthTotal" )
TimeseriesSI( df=allYrSp, yVar="WidthMean" )
TimeseriesSI( df=allYrSp, yVar="AreaTotal" )
TimeseriesSI( df=allYrSp, yVar="LayersMean" )
TimeseriesSI( df=allYrSp, yVar="DateDiff" )
TimeseriesSI( df=allYrSp, yVar="Catch" )

# Plot heatmaps of spawn biomass
PlotBiomassPages <- function( dat, yVar ) {
  # Start the pdf
  pdf( file=file.path(region, paste("HeatLocation", yVar, ".pdf", sep="")), 
      width=figWidth, height=8 )
  # Get the number of pages
  uPages <- unique( dat$SpUnit )
  # Range of SSBs
  rangeY <- range( dat[yVar] )
  # Range of years
  rangeX <- range( dat$Year )
  # Loop over pages
  for( i in 1:length(uPages) ) {
    # Grab a subset of data (required because LocationCode is a number
    df <- dat %>%
        filter( SpUnit == uPages[i] ) %>%
        mutate( LocationCode=factor(LocationCode) )
    # Spawn heatmap
    spawnPlot <- ggplot( data=df, aes(x=Year, y=LocationCode) ) + 
        geom_raster( aes_string(fill=yVar) ) + 
        scale_fill_gradientn( colours=c("blue", "green", "red") ) +
        expand_limits( fill=rangeY, x=rangeX ) +
        scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
        facet_grid( ~ SpUnit, scales="free_y", space="free_y", drop=TRUE ) +
        myTheme
    # Print the plot to the pdf
    print( spawnPlot )
  }  # End i loop over pages
  # Turn the device off
  dev.off( )
}  # End PlotBiomassPages function

## Plot heatmaps
#PlotBiomassPages( dat=siYearLoc, yVar="SITotal" )

BarStatArea <- function( df, yVar ) {
  # Plot spawn biomass by section
  barSectionsSI <- ggplot( data=df, aes_string(x="Year", y=yVar) ) +
      geom_bar( stat="identity", fill="black", col="black", size=0.1 ) + 
      facet_wrap( ~ StatArea, labeller=label_both ) +
      scale_y_continuous( labels=comma ) +
      scale_x_continuous( breaks=seq(from=1000, to=3000, by=20) ) +
      expand_limits( y=0, x=yrRange ) +
      myTheme +
      theme( text=element_text(size=10), panel.spacing=unit(0, "lines") ) +
      ggsave( file=file.path(region, 
              paste("BarSections", yVar, ".pdf", sep="")), width=figWidth, 
          height=figWidth*0.5 )
}  # End BarStatArea function

# Spawn index time series
siPlot <- ggplot( data=filter(allYrSp, !is.na(Survey)), 
        aes(x=Year, group=Survey) ) +
    geom_ribbon( aes(ymin=BiomassLower, ymax=BiomassUpper), fill="lightgrey" ) +
    geom_line( aes(y=BiomassMedian), colour="darkgrey" ) +
    geom_path( aes(y=SITotal) ) +
    geom_point( aes(y=SITotal, shape=Survey) ) +
    scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    labs( y=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")) ) +
    expand_limits( x=yrRange ) +
    myTheme +
    facet_wrap( ~ SpUnit, ncol=1 ) +
    theme( legend.position="top" ) +
    ## Add the reference line if supplied
#if( !is.na(siThreshold) )  siPlot <- siPlot + 
#      geom_hline( yintercept=siThreshold, linetype="dashed", size=0.5 )
    ## Save the plot
#siPlot <- siPlot + 
    ggsave( filename=file.path(region, "SpawnIndex.png"), 
        height=min(8.75, n_distinct(allYrSp$SpUnit)*1.9+1), 
        width=figWidth )

# Weighted mean spawn index by year
wtMeanPlot <- plotMap + 
    geom_point( data=siWeightedYear, aes(colour=SITotal), size=6 ) +
    geom_point( data=siWeightedYear, aes(x=EastingsPrev, y=NorthingsPrev) ) +
    geom_segment( data=siWeightedYear, aes(xend=EastingsPrev, 
            yend=NorthingsPrev) ) +
    scale_colour_distiller( type="seq", palette="Spectral", labels=comma ) +
    facet_wrap( ~ Year, ncol=14 ) +
    ggsave( file=file.path(region, "WeightedMeanSpawnIndex.png"), 
        width=figWidth*14/5, height=figWidth*5/shapes$xyRatio/5 )

# Spawn timing by year and spatial unit
timingPlot <- ggplot( data=filter(siAll, !is.na(Survey)), aes(x=Year) ) +
    geom_point( aes(y=Start, shape=Survey), colour="red" ) +
    geom_point( aes(y=End, shape=Survey), colour="blue" ) +
    scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
    expand_limits( x=yrRange ) +
    labs( y="Date" ) +
    facet_wrap( ~ SpUnit, ncol=1 ) +
    myTheme +
    theme( legend.position="top" ) +
    ggsave( filename=file.path(region, "SpawnTiming.png"), 
        height=min(8.75, n_distinct(siAll$SpUnit)*1.9+1), width=figWidth )


##################
##### Output #####
##################

# Save the workspace image
save.image( file=file.path(region, paste("Image.", region, ".RData", sep="")) ) 


############### 
##### End ##### 
############### 

# Print end of file message and elapsed time
cat( "End of file SpatialAnalysis.R: ", sep="" )
print( Sys.time( ) - sTime2 )
