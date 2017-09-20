
#source("~/CodeTools/mseRtools.r")

# NOTE: You have to run this code twice with iCase=1 (AM1) and iCase=2 (AM2)

# Code for selecting data sets is after functions.
#graphics.off()
#rm(list=ls(all=TRUE))


# Set the working directory.plo
#WD <- "C:/Users/Forrestr/Documents/A_QAMS/Projects/Herring_LRP/A_HerringLRP_NewCode/RefPoints"
WD <- getwd() 
setwd( WD )

# Source functions needed to read iSCAM files.
source( file.path( WD,"read.admb.r" ) )

# Source various R tools for plotting.
#source( file.path( WD,"rTools.r" ) )

# Set to TRUE if an indentifying footer is required on figures.
iFooter <- FALSE

# Settings for *.png files.
xWidth  <- 800
yHeight <- 800
cexSize <- 1.6
ptSize  <- 16

# panLab      (Place text labels in plot region)
# Purpose:    Place a text label in the plot region defined by (0,1), (0,1).
#             The ... notation allows all parameters available to "text" to be
#             passed.
# Parameters: x, y are the coordinates of the label
#             txt is the text
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
# Revised:    K.Holt; 13-Jan-10 to accomodate axes on log scale
panLab <- function( x, y, txt, ... )
{
  # Allows text to be placed in plot panel at 0<x<1, 0<y<1.
  usr <- par( "usr" )
  
  yLog <- par("ylog")
  xLog <- par("xlog")
  
  # Check for log-transformed axes and adjust usr commands as needed
  # note: when a log scale is in use, 
  #           usr gives limits in the form 10 ^ par("usr")
  
  # Case 1: neither axis is on the log scale
  if (yLog==FALSE & xLog==FALSE)
  {
    par( usr=c(0,1,0,1) )
  }
  # Case 2: only the y-axis is on log scale
  if (yLog==TRUE & xLog==FALSE) 
  {
    usr[3:4]<-10 ^ par("usr")[3:4]
    par( usr=c(0,1,0,1), ylog=FALSE )
  } 
  # Case 3: only the x-axis is on log scale
  if (yLog==FALSE & yLog==TRUE) 
  {
    usr[1:2]<-10 ^ par("usr")[1:2]
    par( usr=c(0,1,0,1), xlog=FALSE )
  } 
  # Case 4: both axes are on the log scale
  if (yLog==TRUE & xLog==TRUE) 
  {
    usr[1:4]<-10 ^ par("usr")[1:4]
    par( usr=c(0,1,0,1), xlog=FALSE, ylog=FALSE )
  } 
  text( x, y, txt, ... )
  par( usr=usr )
  return( NULL )
}     # END function panLab


# panLegend   (Place legend in plot region)
# Purpose:    Place a legend in the plot region defined by (0,1), (0,1).
#             The ... notation allows all parameters available to "legend" to be
#             passed.
# Parameters: x, y are the coordinates of the legend
#             legTxt is the text associated with the legend
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
# Revised:    K.Holt; 13-Jan-10 to accomodate axes on log scale
panLegend <- function( x, y, legTxt, ... )
{
  # Allows legend to be placed at 0<x<1, 0<y<1.
  usr <- par( "usr" )
  yLog<-par("ylog")
  xLog<-par("xlog")
  # Check for log-transformed axes and adjust usr commands as needed
  # note: when a log scale is in use, 
  #           usr gives limits in the form 10 ^ par("usr")
  # Case 1: neither axis is on the log scale
  if (yLog==FALSE & xLog==FALSE) {
    par( usr=c(0,1,0,1) )
  }
  # Case 2: only the y-axis is on log scale
  if (yLog==TRUE & xLog==FALSE) 
  {
    usr[3:4]<-10 ^ par("usr")[3:4]
    par( usr=c(0,1,0,1), ylog=FALSE )
  } 
  # Case 3: only the x-axis is on log scale
  if (yLog==FALSE & xLog==TRUE) 
  {
    usr[1:2]<-10 ^ par("usr")[1:2]
    par( usr=c(0,1,0,1), xlog=FALSE )
  } 
  # Case 4: both axes are on the log scale
  if (yLog==TRUE & xLog==TRUE) 
  {
    usr[1:4]<-10 ^ par("usr")[1:4]
    par( usr=c(0,1,0,1), xlog=FALSE, ylog=FALSE )
  } 
  legend( x, y, legend=legTxt, ... )
  par( usr=usr )
  return( NULL )
}     # END function panLegend


plotBiomass <- function( root="inData$bio" )
{
  
  Bt  <- matrix(NA, nrow=5,ncol=length(inData$year))
  SPt <- matrix(NA, nrow=5,ncol=length(inData$year))
  
  par( mfrow=c(3,2), oma=c(3,3,2,2), mar=c(2,2,2,2) )
  for( sID in 1:5 )
  { 
    nam <- paste(root,stockNames[sID],sep="")
    B  <-eval(expr=parse(text=nam))
    #B  <- inData[[i]]
    T  <- length(B)
    nam <- paste("inData$cat",stockNames[sID],sep="")
    kat  <-eval(expr=parse(text=nam))
    Ct <- colSums( kat )
    
    bt      <- B[1:(T-1)]
    btPlus1 <- B[2:T]
    SP     <- btPlus1 - bt + Ct[1:(T-1)] 
    
    Bt[sID,]  <- bt
    SPt[sID,] <- SP 
    
    
    plot(inData[[1]][1:T],B, type="b", 
        main=stockNames[sID],las=1, bty="n",
        xlab="",
        ylab="")
    
  }
  mtext( side=1, text="Year", outer=T, line=0)
  if( root=="inData$ssb" )
    mtext( side=2, text="Spawning biomass", outer=T, line=1)
  else
    mtext( side=2, text="Total biomass", outer=T, line=1)
  
}

plotProduction <- function( root="inData$bio" )
{
  
  Bt  <- matrix(NA, nrow=5,ncol=length(inData$year))
  SPt <- matrix(NA, nrow=5,ncol=length(inData$year))
  
  par( mfrow=c(3,2), oma=c(3,4,2,2), mar=c(2,2,2,2) )
  for( sID in 1:5 )
  { 
    nam <- paste(root,stockNames[sID],sep="")
    B  <-eval(expr=parse(text=nam))
    #B  <- inData[[i]]
    T  <- length(B)
    nam <- paste("inData$cat",stockNames[sID],sep="")
    kat  <-eval(expr=parse(text=nam))
    Ct <- colSums( kat )
    
    bt      <- B[1:(T-1)]
    btPlus1 <- B[2:T]
    SP     <- btPlus1 - bt + Ct[1:(T-1)] 
    
    Bt[sID,]  <- bt
    SPt[sID,] <- SP 
    
    cat( "\nStock:", stockNames[sID], "\n" )
    print( cbind( inData$year, bt,SP) )
    
    plot(bt,SP, axes=F, type="n", 
        main=stockNames[sID],las=1, bty="n",
        xlab="",
        ylab="")
    
    n <- length(bt)     
    ## draw arrows from point to point :
    s <- seq(length(bt)-1)  # one shorter than data
    lineCols1 <- rep( "black", 21 )
    lineCols2 <- rep("gray",T-21+1)
    arrows(bt[s], SP[s], bt[s+1], SP[s+1], length=0.05, col=c(lineCols1,lineCols2))
    s <- s[-length(s)]
    #segments(bt[s], SP[s], bt[s+2], SP[s+2])
    points( bt[1], SP[1], pch=19, col="black", cex=1.5)
    points( bt[n-1], SP[n-1], pch=23, bg="black", cex=1.5)
    abline(h=0) 
    axis(side=2, las=1)  
  }
  if( root=="inData$ssb" )
    mtext( side=1, text="Spawning biomass", outer=T, line=0)
  else
    mtext( side=1, text="Total biomass", outer=T, line=0)
  
  mtext( side=2, text="Surplus production", outer=T, line=1.5)
  
}


getStats <- function( objList )
{
  nStocks <- length( objList )
  #totalBt <- matrix( NA, nrow=nStocks, ncol=length(dat$year) )
  #dimnames( totalBt ) <- list( stockList, paste( dat$year ) )
  
  # Build an empty list to hold results.
  result <- as.list( 1:nStocks )
  names( result ) <- names( objList )
  
  for( i in 1:nStocks )
  {
    # Extract the *.rep file output.
    obj <- objList[[i]]
    
    # Vectors of length(year)
    year  <- obj$yr
    nYrs  <- length( year )
    
    # Total biomass and spawning stock biomass.
    
    # NOTE: I think these include the 1-year forecast, i.e., 1951:(T+1), therefore
    #       since T=nYrs subset the total and spawning biomass to exclude forecast.
    totBt <- obj$bt[ 1:nYrs ]
    SSBt  <- obj$sbt[ 1:nYrs ]
    
    # Revised version (10-Feb-17)
    Dt    <- SSBt / obj$sbo
    
    ut2   <- as.vector(colSums(obj$ut))
    ques  <- obj$q
    
    # Spawn index.
    It    <- obj$it_hat
    # obj$it has an endline at what corresponds to 1987 so that the intended
    # vector gets read in as 2 row by 37 matrix.
    # as.vector rearranges by row, so order of years will be incorrect.
    It    <- as.vector( t( It ) )[1:nYrs]
    
    # Now scale by q's.
    qVec  <- rep( ques[2],length(It) )
    qVec[1:37] <- ques[1]
    Itq <- It / qVec
    
    # Original version.
    #Itq <- Itq[ 1:(nYrs-1) ]
    
    # Observed catch: matrix of 5 rows by length(year) columns for gear types.
    Ctg <- obj$dCatchData %>%
        as_tibble( ) %>%
        rename( Year=V1, Catch=V7 ) %>%
        select( Year, Catch ) %>%
        complete( Year=year, fill=list(Catch=0) ) %>%
        group_by( Year ) %>%
        summarise( Catch=sum(Catch) ) %>%
        ungroup( )
    
    # Sum the catch over gear types.
    Ct <- as.vector( Ctg$Catch )
    
    # Surplus production and production rate based on spawning biomass estimates.
    
    # Original version.
    #ssbt            <- SSBt[ 1:(nYrs-1) ]
    #ssbtPlus1       <- SSBt[ 2:nYrs ]
    
    # Revised version (10-Feb-17).
    ssbt            <- SSBt[ 1:(nYrs-1) ]
    ssbtPlus1       <- SSBt[ 2:nYrs ]
    
    # Natural mortality Added 23-Mar-17
    Mtot        <- obj$M[,1]
    Mtot        <- Mtot[ 1:nYrs ]    
    
    # Original version.
    #surpProdSSB     <- ssbtPlus1 - ssbt + Ct[1:(nYrs-1)]
    
    # Revised version (09-Feb-17)
    surpProdSSB     <- ssbtPlus1 - ssbt + Ct[2:nYrs]
    
    surpProdRateSSB <- surpProdSSB / ssbt
    
    # Surplus production and production rate based on total biomass estimates.
    bt              <- totBt[ 1:(nYrs-1) ]
    btPlus1         <- totBt[ 2:nYrs ]
    
    # Original version.
    #surpProdB       <- btPlus1 - bt + Ct[1:(nYrs-1)]
    
    # Revised version (09-Feb-17)
    surpProdB       <- btPlus1 - bt + Ct[2:nYrs]
    
    surpProdRateB   <- surpProdB / bt
    
    # Surplus production and production rate based on spawner index (q=1).
    it              <- It[ 1:(nYrs-1) ]
    itPlus1         <- It[ 2:nYrs ]
    
    # Original version.
    #surpProdI       <- itPlus1 - it + Ct[1:(nYrs-1)]
    
    # Revised version (09-Feb-17)
    surpProdI       <- itPlus1 - it + Ct[2:nYrs]
    
    surpProdRateI   <- surpProdI / it
    
    group           <- rep( paste( "Dive 1988+ (q=",round(ques[2],digits=3),")", sep="" ),
        length( year ) )
    
    group[1:37]     <- paste( "Surface 1951-1987 (q=",round(ques[1],digits=3),")",sep="" )
    
    # Now pad production estimates since no estimate for nYear.
    surpProdSSB <- c( surpProdSSB, NA )
    surpProdB   <- c( surpProdB, NA )
    surpProdI   <- c( surpProdI, NA )
    
    surpProdRateSSB <- c( surpProdRateSSB, NA )
    surpProdRateB   <- c( surpProdRateB, NA )
    surpProdRateI   <- c( surpProdRateI, NA )    
    
    result[[i]] <- data.frame( year=year, group=group,
        totBt=totBt, SSBt=SSBt, Dt=Dt, ut2=ut2, It=It, qVec=qVec,
        Itq=Itq, Ct=Ct,
        Mt=Mtot, surpProdSSB,     surpProdB,     surpProdI,
        surpProdRateSSB, surpProdRateB, surpProdRateI,
        stringsAsFactors=FALSE )
  }
  result
}     # END function getStats.


# Develop storyboard plot by stock.
plotProdSeries <- function( dat, repFileList, stockName,
    modType=1, prodType="ssb", byGroup=FALSE )
{
  year <- dat[ ,"year" ]
  
  prodI       <- dat[ ,"surpProdI" ]
  prodRateI   <- dat[ ,"surpProdRateI" ]
  
  prodB       <- dat[ ,"surpProdB" ]
  prodRateB   <- dat[ ,"surpProdRateB" ]
  
  prodSSB     <- dat[ ,"surpProdSSB" ]
  prodRateSSB <- dat[ ,"surpProdRateSSB" ]
  
  Ct          <- dat[, "Ct" ]
  Ct[ Ct==0 ] <- NA
  
  It          <- dat[, "It" ]
  SSBt        <- dat[ ,"SSBt" ]
  totBt       <- dat[ ,"totBt" ]
  
  sbo         <- repFileList$sbo
  Blow        <- 0.2 * sbo
  
  sbt         <- repFileList$sbt[1:(length(year)+1)]
  
  # Remember production values have 1 less year than reconstruction, so to match lowest 20th percentiles
  # of SSB must do it prior to shortening index to T-1.
  idq         <- sbt <= quantile( sbt, probs=0.2, na.rm=TRUE )
  idq         <- idq[ 1:(length(idq)-1) ]  
  
  group       <- dat[, "group" ]
  groupList   <- unique( group )
  nGroups     <- length( groupList )
  
  if ( prodType=="total" )
  {
    biomass  <- totBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Total Biomass (000 t)"
    yLab1    <- "Total Production (000 t)"
    yLab2    <- "Total Production Rate"
  }
  
  if ( prodType=="totssb" )
  {
    biomass  <- SSBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Spawning Biomass (000 t)"
    yLab1    <- "Total Production (000 t)"
    yLab2    <- "Total Production Rate"
  }
  
  if ( prodType=="ssb" )
  {
    biomass  <- SSBt
    surpProd <- prodSSB
    prodRate <- prodRateSSB
    xLab     <- "Spawning Biomass (000 t)"
    yLab1    <- "SSB Production (000 t)"
    yLab2    <- "SSB Production Rate"
  }
  
  if ( prodType=="survey" )
  {
    biomass  <- It
    surpProd <- prodI
    prodRate <- prodRateI
    xLab     <- "Survey Biomass (000 t)"
    yLab1    <- "Survey Production (000 t)"
    yLab2    <- "Survey Production Rate"
  }
  
  if ( !byGroup )
    group <- rep( "none", length(group) )
  
  groupList   <- unique( group )
  nGroups     <- length( groupList )
  
  #mfRow <- c( 3,1 )
  #if ( byGroup )
  #  mfRow <- c( 3, nGroups )
  
  par( oma=c(2,1,2,1), mar=c(2,4,1,1), mfrow=c(3,1) )
  
  #for ( i in 1:nGroups )
  {
    # Plot spawning biomass and catch vs. year.
    yLim  <- c( 0, max(SSBt, na.rm=TRUE ) )
    
    #idx <- group==groupList[i]
    
    # Plot spawn index vs. year.
    plot( year, It, type="n", axes=FALSE, xlab="", ylab="" )
    #abline( h=mean(It, na.rm=TRUE ), lty=2, lwd=2 )
    
    idt <- year < 1988
    avgIt <- mean( It[idt] )
    segments( min(year[idt]), avgIt, max(year[idt]), avgIt, lty=2, lwd=2 )
    #lines( year[idt], It[idt], col="gray" )
    
    # ARK 07-Jun-17.
    lines( year[idt], It[idt], col="black" )
    
    points( year[idt], It[idt], cex=cexSize, bg="white", col="black", pch=22 )
    
    # Now add shading to points in the lower 20th percentile.
    points( year[idt & idq ], It[idt & idq], cex=cexSize, bg="gray", col="black", pch=22 )
    
    #cat( "\nSeries\n" )
    #print( cbind( year, biomass, idq ) )
    
    lines( year[idt], mav( It[idt], n=3 ), col="blue", lwd=2 )
    
    idt <- year >= 1988
    avgIt <- mean( It[idt] )
    segments( min(year[idt]), avgIt, max(year[idt]), avgIt, lty=2, lwd=2 )
    lines( year[idt], It[idt], col="black" )
    points( year[idt], It[idt], cex=cexSize, bg="white", col="black", pch=21 )
    
    # Now add shading to points in the lower 20th percentiles.
    points( year[idt & idq], It[idt & idq], cex=cexSize, bg="gray", col="black", pch=21 )
    
    lines( year[idt], mav( It[idt], n=3 ), col="red", lwd=2 )
    
    axis( side=1 )
    axis( side=2, las=2 )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    
    panLab( 0.025, 0.9, cex=1.2, "(a)" ) 
    mtext( side=2, cex.axis=1.2, line=3, "Obs. Spawn Index" )
    
    # Plot production vs. year.
    plot( year, surpProd, type="n", axes=FALSE, xlab="", ylab="" )
    abline( h=0, col="black", lty=3 )
    
    idt <- year < 1988
    
    avgP <- mean( surpProd[idt], na.rm=TRUE )
    #segments( min(year[idt]), avgP, max(year[idt]), avgP, lty=2, lwd=2 )
    lines( year[idt], surpProd[idt] )
    points( year[idt], surpProd[idt], cex=cexSize, bg="white", col="black", pch=22 )
    
    # Now add shading to points in the lower 20th percentile.
    points( year[idt & idq ], surpProd[idt & idq], cex=cexSize, bg="gray", col="black", pch=22 )    
    
    # Moving average.
    lines( year[idt], mav( surpProd[idt], n=3 ), col="blue", lwd=2 )
    
    avgP <- mean( surpProd[!idt], na.rm=TRUE )
    #segments( min(year[!idt]), avgP, max(year[!idt]), avgP, lty=2, lwd=2 )
    lines( year[!idt], surpProd[!idt] )
    points( year[!idt], surpProd[!idt], cex=cexSize, bg="white", col="black", pch=21 )
    
    # Now add shading to points in the lower 20th percentile.
    points( year[!idt & idq ], surpProd[!idt & idq], cex=cexSize, bg="gray", col="black", pch=21 )    
    
    # Moving average.
    lines( year[!idt], mav( surpProd[!idt], n=3 ), col="red", lwd=2 )
    
    axis( side=1 )
    axis( side=2, las=2 )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    
    #mtext( side=1, cex.axis=1.2, line=2, "Year" )
    panLab( 0.025, 0.9, cex=1.2, "(b)" ) 
    mtext( side=2, cex.axis=1.2, line=3, yLab1 )
    
    # Plot production rate vs. year.
    plot( year, prodRate, type="n", axes=FALSE, xlab="", ylab="" )
    abline( h=0, col="black", lty=3 )
    
    avgPB <- mean( prodRate[idt],na.rm=TRUE )
    #segments( min(year[idt]), avgPB, max(year[idt]), avgPB, lty=2, lwd=2 )
    lines( year[idt], prodRate[idt] )
    points( year[idt], prodRate[idt], cex=cexSize, bg="white", col="black", pch=22 )
    
    # Now add shading to points in the lower 20th percentile.
    points( year[idt & idq ], prodRate[idt & idq], cex=cexSize, bg="gray", col="black", pch=22 )    
    
    # Moving average.
    lines( year[idt], mav( prodRate[idt], n=3 ), col="blue", lwd=2 )
    
    avgPB <- mean( prodRate[!idt],na.rm=TRUE )
    #segments( min(year[!idt]), avgPB, max(year[!idt]), avgPB, lty=2, lwd=2 )
    lines( year[!idt], prodRate[!idt] )
    points( year[!idt], prodRate[!idt], cex=cexSize, bg="white", col="black", pch=21 )
    
    # Now add shading to points in the lower 20th percentile.
    points( year[!idt & idq ], prodRate[!idt & idq], cex=cexSize, bg="gray", col="black", pch=21 )    
    
    # Moving average.
    lines( year[!idt], mav( prodRate[!idt], n=3 ), col="red", lwd=2 )
    
    axis( side=1 )
    axis( side=2, las=2 )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    
    panLab( 0.025, 0.9, cex=1.2, "(c)" ) 
    
    mtext( side=2, cex.axis=1.2, line=3, yLab2 )
    
    mtext( side=1, cex.axis=1.2, line=1, outer=TRUE, "Year" )
    
    if ( stockName=="CC8" ) 
      stockName <- "CC"
    
    mtext( side=3, cex.axis=1.4, line=0, outer=TRUE,
        paste( stockName,": AM",modType, sep="" ) )
  }     # for i=1,nGroups
  
  par( mfrow=c(1,1) )
  
  return( invisible() )
}     # END function plotProdSeries


# Develop storyboard plot by stock.
plotModelStates <- function( dat, repFileList, modType=1, stockName, pct=0.2 )
# This function currently plots MLEs.  I suppose that the purists will
# eventually want the mean of the posteriors plotted.
{
  # This can be removed for now, as the full model results can be plotted
  # in this function.
  
  #year        <- dat[ ,"year" ]
  
  #prodI       <- dat[ ,"surpProdI" ]
  #prodRateI   <- dat[ ,"surpProdRateI" ]
  
  #prodB       <- dat[ ,"surpProdB" ]
  #prodRateB   <- dat[ ,"surpProdRateB" ]
  
  #prodSSB     <- dat[ ,"surpProdSSB" ]
  #prodRateSSB <- dat[ ,"surpProdRateSSB" ]
  
  year        <- repFileList$yr
  idxYear     <- repFileList$iyr
  
  # Extract catch and sum over gear types, set 0's to NA for plotting.
  Ctg         <- repFileList$obs_ct
  Ct          <- as.vector( colSums( Ctg ) )
  Ct[ Ct==0 ] <- NA
  
  # Observed spawn index values.
  idt <- year > 1987
  It  <- repFileList$it
  
  # obj$it has an endline at what corresponds to 1987 so that the intended
  # vector gets read in as 2 row by 37 matrix.
  # as.vector rearranges by row, so order of years will be incorrect.
  It   <- as.vector( t( It ) )[1:length(year)]
  
  # Make a vector of q values.
  ques         <- repFileList$q
  qVec         <- rep( ques[2],length(It) )
  qVec[ !idt ] <- ques[1]
  
  # Observed spawn index values div q, I/q.
  Iqt <- It / qVec
  
  # Predicted It, scaled by q's.
  pit <- repFileList$pit
  # obj$pit has an endline at what corresponds to 1987 so that the intended
  # vector gets read in as 2 row by 37 matrix.
  # as.vector rearranges by row, so order of years will be incorrect.
  pit <- as.vector( t( pit ) )[1:length(year)]
  pit <- pit / qVec
  
  ut2 <- repFileList$ut2[1:length(year)]
  
  # fixed cutoff value.
  fixedCutoff <- repFileList$fixed_cutoff
  
  # Absolute recruitments and ln recruitments.
  Rt          <- repFileList$rt[ 1:length(year) ]     # Need to adjust for production offset.
  lnRt        <- repFileList$ln_rt[ 1:length(year) ]
  
  # (ln) Recruitment deviations.
  delta       <- repFileList$delta[1:length(year)]
  
  Mtot        <- repFileList$M_tot[,1]
  Mtot        <- Mtot[ 1:length(year) ]
  
  # Extract wtObs, dropping forecast year and convert to grams.
  wtObs       <- repFileList$wt_obs[ 1:length(year), ]
  wtObs       <- wtObs * 1000.0
  
  SSBt        <- repFileList$sbt[1:length(year)]
  totBt       <- repFileList$bt[1:length(year)]
  
  sbo         <- repFileList$sbo
  
  # Depletion.
  Dept        <- SSBt / sbo
  
  # Lower operational control point (i.e., cut-off).
  Blow        <- c( 0.1, 0.25, 0.3 ) * sbo
  
  par( oma=c(2,1,2,2), mar=c(2,3,1,2), mfcol=c(3,2) )
  
  # Plot total and spawning biomass vs. year.
  yLim  <- c( 0, max(SSBt,na.rm=TRUE) )
  
  plot( year, SSBt, type="n", axes=FALSE, xlab="", ylab="", ylim=yLim )
  #lines( year, totBt )
  #points( year, totBt, bg="black", col="white", pch=21 )
  abline( h=sbo,  col="blue" )
  abline( h=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
  abline( h=Blow,        col=c("red","blue","darkgreen"), lty=.CUTLTY, lwd=.CUTLWD )
  abline( h=0,           col="black", lty=3 )
  
  #abline( h=mean(SSBt), col="black", lty=4, lwd=3 )
  
  lines( year, Ct, type="h", col="black", lwd=3 )
  
  lines( year, SSBt )
  points( year, SSBt, cex=cexSize, bg="white", col="black", pch=21 )
  
  # Indicate SSB values less than specified quantile.
  xQuant <- quantile( SSBt, probs=pct )
  idx <- SSBt <= xQuant
  #cat( "\nModel States\n" )
  #print( cbind( year, SSBt, idx ) )
  points( year[idx], SSBt[idx], cex=cexSize, bg="lightgray", col="black", pch=21 )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  #axis( side=4, labels=FALSE )
  panLab( 0.05, 0.9, cex=1.2, "(a)" ) 
  box()
  
  # Now add depletion - determine how the b0 is calculated.
  par( new=TRUE )
  plot( year, Dept, type="n", axes=FALSE, xlab="",ylab="", ylim=yLim/sbo )
  #lines( year, Dept )
  axis( side=4, las=2 )
  
  #mtext( side=1, cex.axis=1.2, line=2, "Year" )
  mtext( side=2, cex.axis=1.2, line=2.5, "SSBt, Ct (000s t)" )
  #mtext( side=4, cex.axis=1.2, line=3, "Depletion" )
  
  # Plot spawn index vs. year.
  plot( year, Iqt, type="n", axes=FALSE, xlab="", ylab="" )
  #abline( h=mean(It, na.rm=TRUE ), lty=2, lwd=2 )
  
  # Surface spawn index series.
  avgIt <- mean( Iqt[!idt] )
  segments( min(year[!idt]), avgIt, max(year[!idt]), avgIt, lty=2, lwd=2 )
  #lines( year[!idt], Iqt[!idt], col="gray" )
  points( year[!idt], Iqt[!idt], cex=cexSize, bg="gray", col="black", pch=22 )
  
  lines( year[!idt], pit[!idt], col="blue", lwd=2 )
  #lines( year[!idt], mav( Iqt[!idt], n=3 ), col="red", lwd=2 )
  
  # Dive spawn index series.
  avgIt <- mean( Iqt[idt] )
  segments( min(year[idt]), avgIt, max(year[idt]), avgIt, lty=2, lwd=2 )
  #lines( year[idt], Iqt[idt], col="black" )
  points( year[idt], Iqt[idt], cex=cexSize, bg="white", col="black", pch=21 )
  
  lines( year[idt], pit[idt], col="red", lwd=2 )
  #lines( year[idt], mav( Iqt[idt], n=3 ), col="red", lwd=2 )
  
  panLab( 0.25, 0.9, paste( "q1=",round( ques[1], 2 ) ) )
  panLab( 0.75, 0.9, paste( "q2=",round( ques[2], 2 ) ) )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(b)" ) 
  box()
  
  mtext( side=2, cex.axis=1.2, line=2.5, "Spawn Index/q" )
  
  # Plot (ln) recruitment deviations vs. year.  These are age-2s, so need to
  # lag by 1 if you want to plot cohort year.
  
  plot( year, delta, type="n", axes=FALSE, xlab="", ylab="" )
  abline( h=0, lty=2, lwd=2 )
  #abline( h=mean(prodB, na.rm=TRUE ), lty=2, lwd=2 )
  #lines( year, prodB )
  #lines( year, lnRt, type="h", lwd=2 )
  #points( year, delta, cex=cexSize, bg="white", col="black", pch=21 )
  segments( year, 0, year, delta, col="gray", lwd=3 )
  
  # Moving average.
  lines( year, mav( delta, n=3 ), col="red", lwd=2 )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(c)" ) 
  box()
  
  mtext( side=2, cex.axis=1.2, line=2.5, "Rec. Devs. (ln millions)" )
  
  # Estimated natural mortality.
  plot( year, Mtot, type="n", axes=FALSE, xlab="", ylab="", ylim=c(0,max(Mtot)) )
  lines( year, Mtot )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  panLab( 0.05, 0.9, cex=1.2, "(d)" ) 
  box()
  
  mtext( side=4, cex.axis=1.2, line=2, "M")
  
  # Plot weight at age by year.
  #yLim <- c( 0, max( wtObs[,3] ) )
  yLim <- range( wtObs[,1:3])
  plot( range(year), yLim, type="n", axes=FALSE, xlab="", ylab="", ylim=yLim )
  
  for ( j in 1:6 )
  {
    y <- mav( wtObs[,j],n=5 )
    lines( year, y, lty=1, lwd=1 )
    text( 1954, y[6], paste(j+1) )
  }
  lines( year, mav(wtObs[,2], n=5 ), lty=1, lwd=3 )
  
  #points( year, wtObs[,3], cex=cexSize, bg="white", col="black", pch=21 )
  #for ( j in 1:ncol(wtObs) )
  #  lines( year, wtObs[,j] )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(e)" ) 
  box()
  
  mtext( side=4, cex.axis=1.2, line=2, "Weight-at-age (g)")
  
  # Harv.
  plot( year, ut2, type="n", axes=FALSE, xlab="", ylab="" )
  
  if ( stockName=="CC8" || stockName=="CC67")
  {
    segments( c(1983,2014,2015,2016), c(0.2,0.2,0.1,0.07),
        c(2014,2015,2016,2017), c(0.2,0.1,0.07,0.07),
        col="black", lty=3, lwd=2 )
  }
  else
  {
    #abline( h=0.2, col="black", lty=3, lwd=2 )
    segments( 1983, 0.2, 2016, 0.2, col="black", lty=3, lwd=2 )
  }
  
  # 20% harvest rate introduced 1983.
  
  yr1 <- 1983
  yr2 <- max( year )
  if ( stockName=="CC8" )
    yr2 <- 2007
  if ( stockName=="CC67" )
    yr2 <- 2007
  
  if ( stockName=="HG" )
    yr2 <- 2002
  
  if ( stockName=="PRD" )
    yr1 <- 1984
  
  if ( stockName=="WCVI" )
    yr2 <- 2005
  
  id1 <- which( year==yr1, arr.ind=TRUE )
  id2 <- which( year==yr2, arr.ind=TRUE )
  uMean <- mean( ut2[ c(id1:id2) ] )
  cat( "\nStock ",stockName," uMean = ",uMean,"\n" )
  segments( yr1, uMean, yr2, uMean, col="blue", lwd=3 )
  
  lines( year, ut2 )
  
  ut2[ ut2==0 ] <- NA
  points( year, ut2, cex=cexSize, bg="white", col="black", pch=21 )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(f)" ) 
  box()
  
  mtext( side=4, cex.axis=1.2, line=2, "Harvest Rate" )
  
  mtext( side=1, cex.axis=1.2, line=1, outer=TRUE, "Year" )
  
  if ( stockName=="CC8" ) 
    stockName <- "CC"
  mtext( side=3, cex.axis=1.4, line=0, outer=TRUE,
      paste( stockName,": AM",modType, sep="" ) )
  
  par( mfrow=c(1,1) )
}     # END function plotModelStates


plotProduction <- function( dat, repFileList, stockName, 
    modType=1, prodType="total", byGroup=FALSE,
    xyScale=FALSE )
{
  year        <- dat[ ,"year" ]
  sbo         <- repFileList$sbo
  fixedCutoff <- repFileList$fixed_cutoff
  
  prodI       <- dat[ ,"surpProdI" ]
  prodRateI   <- dat[ ,"surpProdRateI" ]
  
  prodB       <- dat[ ,"surpProdB" ]
  prodRateB   <- dat[ ,"surpProdRateB" ]
  
  prodSSB     <- dat[ ,"surpProdSSB" ]
  prodRateSSB <- dat[ ,"surpProdRateSSB" ]
  
  It          <- dat[, "It" ]
  SSBt        <- dat[ ,"SSBt" ]
  totBt       <- dat[ ,"totBt" ]
  
  group       <- dat[, "group" ]
  
  if ( !byGroup )
    group <- rep( "none", length(group) )
  
  groupList   <- unique( group )
  nGroups     <- length( groupList )
  
  mfCol <- c( 2,1 )
  if ( byGroup )
    mfCol <- c( 2, nGroups )
  
  if ( prodType=="total" )
  {
    biomass  <- totBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Total Biomass (000 t)"
    yLab1    <- "Total Production (000 t)"
    yLab2    <- "Total Production Rate"
  }
  
  if ( prodType=="totssb" )
  {
    biomass  <- SSBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Spawning Biomass (000 t)"
    yLab1    <- "Total Production (000 t)"
    yLab2    <- "Total Production Rate"
  }
  
  if ( prodType=="ssb" )
  {
    biomass  <- SSBt
    surpProd <- prodSSB
    prodRate <- prodRateSSB
    xLab     <- "Spawning Biomass (000 t)"
    yLab1    <- "SSB Production (000 t)"
    yLab2    <- "SSB Production Rate"
  }
  
  if ( prodType=="survey" )
  {
    biomass  <- It
    surpProd <- prodI
    prodRate <- prodRateI
    xLab     <- "Survey Biomass (000 t)"
    yLab1    <- "Survey Production (000 t)"
    yLab2    <- "Survey Production Rate"
  }
  
  par( oma=c(2,2.5,3,1), mar=c(2,2,2,1), mfcol=mfCol )
  
  for ( i in 1:nGroups )
  {
    idx    <- group==groupList[i]
    b      <- biomass[idx]
    SP     <- surpProd[idx]
    SPrate <- prodRate[idx]
    yr     <- year[idx]
    
    n      <- length( yr )
    
    if ( xyScale )
    {
      xLim  <- c( 0,max(b,na.rm=TRUE) )
      yLim1 <- range( SP, na.rm=TRUE ) * 1.05
      yLim2 <- range( SPrate, na.rm=TRUE ) *1.05
    }
    else
    {
      xLim   <- c( 0,max(biomass, na.rm=TRUE) )
      yLim1  <- range( surpProd, na.rm=TRUE )*1.05
      yLim2  <- range( prodRate, na.rm=TRUE )*1.05
    }
    
    # Phase plot surplus production versus biomass.
    plot( b, SP, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim1 )
    
    ## Draw arrows from point to point, not sure why "21", would have thought
    #  should be 1988 when dive surveys started.
    
    # Lay down depletions at 0.1, 0.25, 0.3 of B0.
    deps <- c( 0.1, 0.25, 0.3 ) * sbo
    abline( v=deps, lty=3, lwd=2, col=c( "red","blue","darkgreen") )
    abline( v=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
    
    lineCols <- rep( "black", length(b) )
    s <- seq( length(b)-1 )
    arrows( b[s], SP[s], b[s+1], SP[s+1], length=0.05, col=lineCols )
    
    # Lay down heat colors for order of points.
    #colVec <- rev( heat.colors( n ) )
    colVec <- rev( gray.colors( n ) )
    points( b, SP, cex=cexSize, pch=21, bg=colVec )
    
    points( b[1], SP[1], cex=cexSize, pch=19, col="black" )
    points( b[n-1], SP[n-1], cex=cexSize, pch=23, bg="black" )
    
    text( b, SP, labels=yr, adj=1, cex=0.7, pos=3 )
    
    abline( h=0 )
    
    axis( side=1 ) 
    axis( side=2, las=2 )
    #axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    
    par( new="TRUE" )
    plot( xLim/sbo, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
    axis( side=3 )
    
    box()
    
    if ( nGroups > 1 )
    {
      mfg <- par( "mfg" )
      if ( mfg[1]==1 )
        mtext( side=3, cex=1, line=2, groupList[i] )
    }
    
    mfg <- par( "mfg" )
    if ( mfg[2]==1 )
      mtext( side=2, cex=1.2, line=3, yLab1 )
    
    # Phase plot production rate versus biomass.
    yLim <- range( prodRate, na.rm=TRUE )
    plot( b,SPrate, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim2 )
    
    ## Draw arrows from point to point, not sure why "21", would have thought
    #  should be 1988 or "38" when dive surveys started.
    
    lineCols <- rep( "black", length(b) )
    s <- seq( length(b)-1 )
    arrows( b[s], SPrate[s], b[s+1], SPrate[s+1], length=0.05, col=lineCols )
    
    # Lay down heat colors for order of points.
    #colVec <- rev( heat.colors( n ) )
    points( b,    SPrate,    cex=cexSize, pch=21, bg=colVec )
    points( b[1], SPrate[1], cex=cexSize, pch=19, col="black" )
    points( b[n-1], SPrate[n-1], cex=cexSize, pch=23, bg="black"  )
    
    text( b, SPrate, labels=yr, adj=1, cex=0.7, pos=3 )
    
    abline( h=0 )
    
    axis( side=1 ) 
    axis( side=2, las=2 )
    #axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    
    par( new="TRUE" )
    plot( xLim/sbo, yLim2, type="n", axes=FALSE, xlab="", ylab="" )
    axis( side=3 )
    
    box()
    
    if ( mfg[2]==1 )
      mtext( side=2, cex=1.2, line=3, yLab2 )
  }     # for i=1,nGroups
  
  mtext( side=1, cex=1.2, line=0.5, outer=TRUE, xLab )
  
  if ( stockName=="CC8" ) 
    stockName <- "CC"
  
  mtext( side=3, cex=1.3, line=1,   outer=TRUE, 
      paste( stockName,": AM",modType, sep="" ) )
  
  par( mfrow=c(1,1) )
  return( invisible() )
}     # END function plotProduction

plotProbProd <- function( dat, repFileList, stockName, prodType="total", xyScale=FALSE )
{
  # Sort in ascending order by year.
  dat         <- dat[ order(dat[,"year"]), ]
  
  year        <- dat[ ,"year" ]
  sbo         <- repFileList$sbo
  fixedCutoff <- repFileList$fixed_cutoff
  
  prodI       <- dat[ ,"surpProdI" ]
  prodRateI   <- dat[ ,"surpProdRateI" ]
  
  prodB       <- dat[ ,"surpProdB" ]
  prodRateB   <- dat[ ,"surpProdRateB" ]
  
  prodSSB     <- dat[ ,"surpProdSSB" ]
  prodRateSSB <- dat[ ,"surpProdRateSSB" ]
  
  It          <- dat[, "It" ]
  SSBt        <- dat[ ,"SSBt" ]
  totBt       <- dat[ ,"totBt" ]
  
  group       <- dat[, "group" ]
  
  if ( prodType=="total" )
  {
    biomass  <- totBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Total Biomass (000 t)"
    yLab     <- "Total Production (000 t)"
  }
  
  if ( prodType=="totssb" )
  {
    biomass  <- SSBt
    surpProd <- prodB
    prodRate <- prodRateB
    xLab     <- "Spawning Biomass (000 t)"
    yLab     <- "Pr( Negative Production After Year )"
  }
  
  if ( prodType=="ssb" )
  {
    biomass  <- SSBt
    surpProd <- prodSSB
    prodRate <- prodRateSSB
    xLab     <- "Spawning Biomass (000 t)"
    yLab     <- "Pr( Negative Production After Year )"
  }
  
  if ( prodType=="survey" )
  {
    biomass  <- It
    surpProd <- prodI
    prodRate <- prodRateI
    xLab     <- "Survey Biomass (000 t)"
    yLab     <- "Pr( Negative Production After Year )"
  }
  
  b      <- biomass
  SP     <- surpProd
  SPrate <- prodRate
  yr     <- year
  
  n      <- length( yr )
  
  # Now calculate proportion of years with negative production that follow (could do below avg too).
  # How about just over the next 10 years?
  
  #prodVal  <- SP < quantile(SP, probs=1/3 )
  prodVal  <- SP < 0
  probNeg  <- rep( NA, n )
  nYears   <- rep( NA, n )
  negYears <- rep( NA, n )
  for ( i in 1:n )
  {
    val         <- prodVal[ (i+1):n ]
    #val         <- prodVal[ (i+1):(i+15)]
    
    nYears[i]   <- length( val )
    negYears[i] <- sum( val )
    
    probNeg[i]  <- negYears[i] / nYears[i]
  }
  
  #print( cbind( year,negYears,nYears, probNeg ) )
  
  if ( xyScale )
  {
    xLim <- c( 0,max(b, na.rm=TRUE) )
    yLim <- c(0,1)
  }
  else
  {
    xLim  <- c( 0,max(biomass,na.rm=TRUE) )
    yLim  <- c(0,1)
  }
  
  par( oma=c(2,2.5,3,1), mar=c(2,2,2,1), mfcol=c(1,1) )
  
  # Plot proportion of negative years for each year.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
  
  # Lay down depletions at 0.1, 0.25, 0.3 of B0.
  deps <- c( 0.1, 0.25, 0.3 ) * sbo
  abline( v=deps, lty=3, lwd=2, col=c( "red","blue","darkgreen") )
  abline( v=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
  
  # Lay down heat colors for order of points.
  #colVec <- rev( heat.colors( n ) )
  colVec <- rev( gray.colors( n ) )
  points( b, probNeg, cex=cexSize, pch=21, bg=colVec )
  
  points( b[1], probNeg[1], cex=cexSize, pch=19, col="black" )
  points( b[n-1], probNeg[n-1], cex=cexSize, pch=23, bg="black" )
  
  text( b, probNeg, labels=yr, adj=1, cex=0.7, pos=3 )
  
  abline( h=0 )
  
  axis( side=1 ) 
  axis( side=2, las=2 )
  #axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  par( new="TRUE" )
  plot( xLim/sbo, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  axis( side=3 )
  
  box()
  
  mtext( side=1, cex=1.2, line=0.5, outer=TRUE, xLab )
  mtext( side=2, cex=1.2, line=1,   outer=TRUE, yLab )
  
  if ( stockName=="CC8" ) 
    stockName <- "CC"
  
  mtext( side=3, cex=1.3, line=1,   outer=TRUE, stockName )
  
  return( invisible() )
}     # END function plotProbProd

# Function to smooth using a trailing moving average of length n.
mav <- function( x, n=5) { filter( x, rep(1/n,n), sides=1 ) }

plotYield <- function( obj, stockName )
{
  # Observed catch: matrix of 5 rows by length(year) columns for gear types.
  Ctg   <- obj$obs_ct
  # Sum the catch over gear types.
  Ct <- as.vector( colSums( Ctg ) )  
  sbt <- obj$sbt
  
  # Years of reconstruction.
  yr  <- obj$yr
  nYr <- length( yr )
  
  Ct  <- Ct[ 1:nYr ]
  sbt <- sbt[ 1:nYr ]
  
  # Remove 0 catch years?
  
  plot( sbt, Ct )
  
  # Lay down heat colors for order of points.
  #colVec <- rev( heat.colors( n ) )
  colVec <- rev( gray.colors( nYr ) )
  points( sbt, Ct, cex=cexSize, pch=21, bg=colVec )
  
  points( sbt[1], Ct[1], cex=cexSize, pch=19, col="black" )
  points( sbt[nYr-1], Ct[nYr-1], cex=cexSize, pch=23, bg="black" )
  
  text( sbt, Ct, labels=yr, adj=1, cex=0.7, pos=3 )  
}

#--------------------------------------------------------------------#

# 1. DONE: Need to split surface survey from dive survey period.  The biomass
#    estimates are very likely scaled differently.  Also, conditions in
#    the past are not the same as conditions now.
# 2. Reduction fishery from 1950 to 1970.  Roe fishery 1971+.
#    Maybe have some shading to indicate?
# 3. DONE: Surface surveys 1950 to 1987.  Dive surveys 1988+.
# 4. DONE: Need to revise code to read *.rep files.  This is because you want:
#    SSB, Bt, Bt3+, It/q, where q=1 options, and the production estimates
#    for each, as well as reference points, depletion etc.
# 5. DONE: Then you need to be able to specify groups of years to split out plots
#    with labels on the years.  I would worry about combining the plots
#    later, just label the year range now, i.e., "WCVI 1950-1970".
# 6. DONE: Add a moving average filter to the production and production rate
#    time-series.
# 7. Ensure that phase and time-series plots are done with same base for
#    production estimates.
# 8. DONE: Add recruitment deviations to plot.
# 9. Add estimates of M if time-varying.
#10. Add mean weight at age, and/or Walford parameters, tho the former may be
#    stickier.

# This reads in the data Cox used, replace with reads from *.rep files.
#inData <- lisread( "herrSP.dat" )
#stockNames <- c( "SOG","CC","PRD","WCVI","HG" )

# Read in *.rep files for each stock.
#mgmtAreas <- c( "SOG","CC","PRD","WCVI","HG" )
#baseName <- "Herring2015qBaseGeneric-pfcB"

#repFiles <- paste( mgmtAreas, baseName, ".rep", sep="" )

#repList <- as.list( 1:length( mgmtAreas ) )
#for ( i in 1:length(repList) )
#{
#  repList[[i]] <- read.rep( repFiles[i] )
#}

# Note use of stockNames here to replace QCI with HG.
#names( repList ) <- stockNames

plotSAR <- function( dat, repFileList, stockName, 
    modType=1, prodType="ssb", byGroup=FALSE,
    xyScale=FALSE )
{
  year        <- dat[ ,"year" ]
  sbo         <- repFileList$sbo
  fixedCutoff <- repFileList$fixed_cutoff
  
  prodI       <- dat[ ,"surpProdI" ]
  prodRateI   <- dat[ ,"surpProdRateI" ]
  
  prodB       <- dat[ ,"surpProdB" ]
  prodRateB   <- dat[ ,"surpProdRateB" ]
  
  prodSSB     <- dat[ ,"surpProdSSB" ]
  prodRateSSB <- dat[ ,"surpProdRateSSB" ]
  
  It          <- dat[, "It" ]
  SSBt        <- dat[ ,"SSBt" ]
  totBt       <- dat[ ,"totBt" ]
  
  Ct          <- dat[, "Ct" ]
  Mtot        <- dat[, "Mt" ]
  
  # This is surface vs. dive group.
  group       <- dat[, "group" ]
  
  groupList   <- unique( group )
  nGroups     <- length( groupList )
  
  qVec        <- dat[ ,"qVec" ]
  qList       <- unique( qVec )
  
  if ( prodType=="ssb" )
  {
    biomass  <- SSBt
    surpProd <- prodSSB
    prodRate <- prodRateSSB
    xLab     <- "Spawning Biomass (000 t)"
    yLab1    <- "Spawning Biomass Production (000 t)"
    yLab2    <- "Spawning Biommss Production Rate"
  }
  
  # Depletion.
  Dept        <- SSBt / sbo
  
  # Lower operational control point (i.e., cut-off).
  Blow        <- c( 0.1, 0.25, 0.3 ) * sbo
  
  # Plot total and spawning biomass vs. year.
  yLim  <- c( 0, max(SSBt,na.rm=TRUE) )
  
  par( oma=c(1.5,0.5,3,1), mar=c(2,4,2,0.5), mfcol=c(2,2) )
  
  # Plot spawning biomass trajectory.
  plot( year, SSBt, type="n", axes=FALSE, xlab="", ylab="", ylim=yLim )
  #lines( year, totBt )
  #points( year, totBt, bg="black", col="white", pch=21 )
  abline( h=sbo,  col="blue" )
  abline( h=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
  abline( h=Blow,        col=c("red","blue","darkgreen"), lty=.CUTLTY, lwd=.CUTLWD )
  abline( h=0,           col="black", lty=3 )
  
  #abline( h=mean(SSBt), col="black", lty=4, lwd=3 )
  
  # Sub NAs for 0s to remove plotting artefact.
  tmpCt <- Ct
  tmpCt[ tmpCt==0 ] <- NA
  lines( year, tmpCt, type="h", col="black", lwd=3 )
  
  lines( year, SSBt )
  points( year, SSBt, cex=cexSize, bg="white", col="black", pch=21 )
  
  # Indicate SSB values less than specified quantile.
  xQuant <- quantile( SSBt, probs=0.2 )
  idx <- SSBt <= xQuant
  #cat( "\nModel States\n" )
  #print( cbind( year, SSBt, idx ) )
  points( year[idx], SSBt[idx], cex=cexSize, bg="lightgray", col="black", pch=21 )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  panLab( 0.05, 0.9, cex=1.2, "(a)" ) 
  box()
  
  # Now add depletion - determine how the b0 is calculated.
  #par( new=TRUE )
  #plot( year, Dept, type="n", axes=FALSE, xlab="",ylab="", ylim=yLim/sbo )
  #lines( year, Dept )
  #axis( side=4, las=2 )
  
  #mtext( side=1, cex.axis=1.2, line=2, "Year" )
  mtext( side=2, cex=1.1, line=2.5, "Spawning biomass, Catch (000 t)" )
  #mtext( side=4, cex.axis=1.2, line=3, "Depletion" )  
  
  # Plot estimated natural mortality.
  plot( year, Mtot, type="n", axes=FALSE, xlab="", ylab="", ylim=c(0,max(Mtot)) )
  lines( year, Mtot, lwd=2 )
  
  axis( side=1 )
  axis( side=2, las=2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  panLab( 0.05, 0.9, cex=1.2, "(b)" ) 
  box()
  
  mtext( side=1, cex=1.1, line=2, "Year" )
  mtext( side=2, cex=1.1, line=2.5, "Natural mortality" )
  
  # Plotting variables for phase plots is Dive only.
  
  idx    <- group==groupList[2]
  b      <- biomass[idx]
  SP     <- surpProd[idx]
  SPrate <- prodRate[idx]
  yr     <- year[idx]
  
  n      <- length( yr )
  
  if ( xyScale )
  {
    xLim  <- c( 0,max(b,na.rm=TRUE) )
    yLim1 <- range( SP, na.rm=TRUE ) * 1.05
    yLim2 <- range( SPrate, na.rm=TRUE ) *1.05
  }
  else
  {
    xLim   <- c( 0,max(biomass, na.rm=TRUE) )
    yLim1  <- range( surpProd, na.rm=TRUE )*1.05
    yLim2  <- range( prodRate, na.rm=TRUE )*1.05
  }
  
  # Phase plot surplus production versus biomass.
  plot( b, SP, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim1 )
  
  # Lay down depletions at 0.1, 0.25, 0.3 of B0.
  deps <- c( 0.1, 0.25, 0.3 ) * sbo
  abline( v=deps, lty=3, lwd=2, col=c( "red","blue","darkgreen") )
  abline( v=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
  
  lineCols <- rep( "black", length(b) )
  s <- seq( length(b)-1 )
  arrows( b[s], SP[s], b[s+1], SP[s+1], length=0.05, col=lineCols )
  
  # Lay down heat colors for order of points.
  #colVec <- rev( heat.colors( n ) )
  colVec <- rev( gray.colors( n ) )
  points( b, SP, cex=cexSize, pch=21, bg=colVec )
  
  points( b[1], SP[1], cex=cexSize, pch=19, col="black" )
  points( b[n-1], SP[n-1], cex=cexSize, pch=23, bg="black" )
  
  text( b, SP, labels=yr, adj=1, cex=0.7, pos=3 )
  
  abline( h=0 )
  
  axis( side=1 ) 
  axis( side=2, las=2 )
  #axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(c)" )
  
  par( new="TRUE" )
  plot( xLim/sbo, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  axis( side=3 )
  
  box()
  
  mfg <- par( "mfg" )
  #if ( mfg[2]==1 )
  mtext( side=2, cex=1.1, line=2.5, yLab1 )
  
  if ( mfg[1]==1 )
    mtext (side=3, cex=1.1, line=2, "Spawning Biomass Depletion" )
  #mtext( side=3, cex=1, line=2, groupList[2] )
  
  # Phase plot production rate versus biomass.
  yLim <- range( prodRate, na.rm=TRUE )
  plot( b,SPrate, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim2 )
  
  ## Draw arrows from point to point, not sure why "21", would have thought
  
  lineCols <- rep( "black", length(b) )
  s <- seq( length(b)-1 )
  arrows( b[s], SPrate[s], b[s+1], SPrate[s+1], length=0.05, col=lineCols )
  
  # Lay down heat colors for order of points.
  #colVec <- rev( heat.colors( n ) )
  points( b,    SPrate,    cex=cexSize, pch=21, bg=colVec )
  points( b[1], SPrate[1], cex=cexSize, pch=19, col="black" )
  points( b[n-1], SPrate[n-1], cex=cexSize, pch=23, bg="black"  )
  
  text( b, SPrate, labels=yr, adj=1, cex=0.7, pos=3 )
  
  abline( h=0 )
  
  axis( side=1 ) 
  axis( side=2, las=2 )
  #axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  panLab( 0.05, 0.9, cex=1.2, "(d)" )
  
  par( new="TRUE" )
  plot( xLim/sbo, yLim2, type="n", axes=FALSE, xlab="", ylab="" )
  axis( side=3 )
  
  box()
  
  #if ( mfg[2]==1 )
  mtext( side=2, cex=1.1, line=2.5, yLab2 )
  
  mtext( side=1, cex=1.1, line=2, xLab )
  
  if ( stockName=="CC8" ) 
    stockName <- "CC"
  
  mtext( side=3, cex=1.25, line=1,   outer=TRUE,
      paste( stockName,": AM",modType, sep="" ) ) 
  #paste( stockName,": AM",modType,"(",qList[2],")", sep="" ) )
  
  par( mfrow=c(1,1) )
  return( invisible() )
}     # END function plotSAR

## Vector of model names
#iCase <- 2
# Set the model case folder (AM1=1 or AM2=2)
#  caseFld  <- "AM1(martell)_5,000,000"

#  baseName <- "Herring2016qEst"
baseName <- "iscam"

#if ( iCase==2 )
#{
##  caseFld  <- "AM2(historical)_5,000,000"
#  caseFld  <- "AM2"
##  baseName <- "Herring2016qBaseGeneric-pfcB"
#  baseName <- "iscam"
#}

#fileList <- list.files( caseFld )

##### Start a loop over models (cases) #####
for( m in 1:length(mNames) ) {
  
  # Get the model name
  caseFld  <- mNames[m]
  
# There are 2 Central Coast prefixes to account for the area adjustment cases,
# but they are not named uniquely so a few lines down that is fixed when
# mgmtAreas is patched and used to name the repList of *.rep file contents.
mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI")
fileList <- mgmtAreas
#repFiles <- paste( mgmtAreas, baseName, ".rep", sep="" )
repFiles <- paste( baseName, ".rep", sep="" )

# Read in *.rep files for each stock.
repList <- as.list( 1:length( mgmtAreas ) )
for ( i in 1:length(repList) )
{
#  fName <- file.path( fileList[i], caseFld, repFiles[i] )
  fName <- file.path( fileList[i], caseFld, repFiles )
  cat( "\nReading ",fName,"\n" )
  repList[[i]] <- read.rep( fName )
  cat( "\nDone!\n" )
}

#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI" )
names( repList ) <- mgmtAreas

# Fixed cutoff values - these are actually in the rep files:
# SOG = 21,200
# CC  = 17,600
# PRD = 12,100
# WCVI = 18,800
# HG  = 10,700
#cutOffs <- c( 21200, 17600, 12100, 18800, 10700 ) / 1000.0

.CUTCOL <- "black"
.CUTLTY <- 3
.CUTLWD <- 2

.FIXCOL <- "blue"
.FIXLTY <- 2
.FIXLWD <- 3

# Plotting options.
prodType <- "ssb"
byGrp    <- TRUE
xyScale  <- TRUE

cat( "\nGathering production statistics...\n" )
result     <- getStats( repList )
stockNames <- names( result )

for ( i in 1:length( result ) )
{
  cat( "\nMSG: Plotting Stock = ",stockNames[i], "\n" )
  cat( "\nEstimated q values:", repList[[i]]$q, "\n" )
  
  # Yield curves.
  #fName <- paste( "yield",stockNames[i],".png", sep="" )
  #png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
  #plotYield( repList[[i]], stockName=stockNames[i] )
  #mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
  #dev.off()
  
#  # Phase plots of production and production rate.
#  # Output to png.
#  if ( xyScale )
#    fName <- paste( "phaseAM",iCase,stockNames[i],".png", sep="" )
#  else
#    fName <- paste( "phaseFixAM",iCase,stockNames[i],".png", sep="" )
#
#  png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
#  plotProduction( result[[i]], repList[[i]], stockName=stockNames[i],
#    modType=iCase, prodType=prodType, byGroup=byGrp, xyScale=xyScale )
#
#  if ( iFooter )
#    mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
#  dev.off()
  
  # Figures for SAR.
  fName <- file.path( stockNames[i], 
      paste( "Production",caseFld,".png", sep="" ) )
  png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
  plotSAR( result[[i]], repList[[i]], stockName=stockNames[i],
      modType=caseFld, prodType=prodType, byGroup=byGrp, xyScale=xyScale )
  
  if ( iFooter )
    mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
  dev.off()   
  
#  # Time-series of production and production rate.
#  fName <- paste( "seriesAM",iCase,stockNames[i],".png", sep="" )
#  png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize+2 )
#  plotProdSeries( result[[i]], repList[[i]], stockName=stockNames[i],
#    modType=iCase, prodType=prodType, byGroup=TRUE )
#
#  if ( iFooter )
#    mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
#  dev.off()
  
#  # Model states storyboard.
#  fName <- paste( "statesAM",iCase,stockNames[i],".png", sep="" )
#  png( file=fName, height=yHeight, width=1024, pointsize=ptSize+2 )
#  plotModelStates( result[[i]], repList[[i]],
#  	               modType=iCase, stockName=stockNames[i] )
#
#  if ( iFooter )
#    mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
#  dev.off()
  
  # Probability of negative production plots.
  #fName <- paste( "probProdAM",iCase,stockNames[i],".png", sep="" )
  #png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
  #plotProbProd( result[[i]], repList[[i]], stockName=stockNames[i], prodType=prodType )
  #dev.off()  
}

##### End m loop over models (cases) #####
}

## ARK (01-Dec-16) Change to save both repLists for plotting.
#
## Read AM1 files.
#caseFld  <- "AM1(martell)_5,000,000"
#baseName <- "Herring2016qEst"
#fileList <- list.files( caseFld )
#
## There are 2 Central Coast prefixes to account for the area adjustment cases,
## but they are not named uniquely so a few lines down that is fixed when
## mgmtAreas is patched and used to name the repList of *.rep file contents.
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI")
#repFilesAM1 <- paste( mgmtAreas, baseName, ".rep", sep="" )
#
## Read in *.rep files for each stock.
#repListAM1 <- as.list( 1:length( mgmtAreas ) )
#for ( i in 1:length(repListAM1) )
#{
#  repListAM1[[i]] <- read.rep( file.path( caseFld, fileList[i], repFilesAM1[i] ) )
#}
#
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI" )
#names( repListAM1 ) <- mgmtAreas
#
#resultAM1  <- getStats( repListAM1 )
#stockNames <- names( resultAM1 )
#
## Output *.csv files.
#fName <- paste( "prodDataAM1",stockNames,".csv", sep="" )
#for ( i in 1:length(resultAM1) )
#{
#  cat( "\nWriting to ",fName,"\n" )
#  headers <- c( "year","group","SSBt","Dt","It","Itq","Ct","ut2","surpProdSSB","surpProdRateSSB")
#  foo <- resultAM1[[i]]
#  foo <- foo[,headers ]
#  foo$group <- ifelse( substring( foo$group,1,4 )=="Dive", "Dive","Surface" )
#  #foo$q20 <- ifelse( foo$SSBt <= quantile( foo$SSBt, probs=0.2 ), 1,0 )
#  foo$qSSB <-  cut( foo$SSBt, breaks=c(quantile( foo$SSBt, probs=seq(0,1,0.1) )),
#                    include.lowest=TRUE, labels=paste( c(1:10) ) )
#  foo$qProd <-  cut( foo$surpProdSSB, breaks=c(quantile( foo$surpProdSSB, probs=seq(0,1,0.1), na.rm=TRUE )),
#                     include.lowest=TRUE, labels=paste( c(1:10) ) )
#  write.csv( foo, file=fName[i], row.names=FALSE )
#}
#
## Read AM2 files.
#caseFld  <- "AM2(historical)_5,000,000"
#baseName <- "Herring2016qBaseGeneric-pfcB"
#fileList <- list.files( caseFld )
#
## There are 2 Central Coast prefixes to account for the area adjustment cases,
## but they are not named uniquely so a few lines down that is fixed when
## mgmtAreas is patched and used to name the repList of *.rep file contents.
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI")
#repFilesAM2 <- paste( mgmtAreas, baseName, ".rep", sep="" )
#
## Read in *.rep files for each stock.
#repListAM2 <- as.list( 1:length( mgmtAreas ) )
#for ( i in 1:length(repListAM2) )
#{
#  repListAM2[[i]] <- read.rep( file.path( caseFld, fileList[i], repFilesAM2[i] ) )
#}
#
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI" )
#names( repListAM2 ) <- mgmtAreas
#
#resultAM2  <- getStats( repListAM2 )
#stockNames <- names( resultAM2 )
#
## Output *.csv files.
#fName <- paste( "prodDataAM2",stockNames,".csv", sep="" )
#for ( i in 1:length(resultAM2) )
#{
#  cat( "\nWriting to ",fName,"\n" )
#  headers <- c( "year","group","SSBt","Dt","It","Itq","Ct","ut2","surpProdSSB","surpProdRateSSB")
#  foo <- resultAM2[[i]]
#  foo <- foo[,headers ]
#  foo$group <- ifelse( substring( foo$group,1,4 )=="Dive", "Dive","Surface" )
#  #foo$q20 <- ifelse( foo$SSBt <= quantile( foo$SSBt, probs=0.2 ), 1,0 )
#  foo$qSSB <-  cut( foo$SSBt, breaks=c(quantile( foo$SSBt, probs=seq(0,1,0.1) )),
#                    include.lowest=TRUE, labels=paste( c(1:10) ) )
#  foo$qProd <-  cut( foo$surpProdSSB, breaks=c(quantile( foo$surpProdSSB, probs=seq(0,1,0.1), na.rm=TRUE )),
#                     include.lowest=TRUE, labels=paste( c(1:10) ) )  
#  write.csv( foo, file=fName[i], row.names=FALSE )
#}
#
## Now plot 2 (AM1,AM2) by 2 (Surface, Dive) plots for MLEs.
#
#plotProdAM12 <- function( dat1, repList1, dat2, repList2, stockName,
#                  prodType="total", xyScale=FALSE )
#{
#  # Extract AM1 quantities.
#  
#  year        <- dat1[ ,"year" ]
#  sbo         <- repList1$sbo
#  fixedCutoff <- repList1$fixed_cutoff
#  depCutoff   <- fixedCutoff / sbo
#  cat( "\nStock ",stockName," depletion at fixedCutoff = ",
#       depCutoff, "\n" )
#
#  prodI       <- dat1[ ,"surpProdI" ]
#  prodRateI   <- dat1[ ,"surpProdRateI" ]
#
#  prodB       <- dat1[ ,"surpProdB" ]
#  prodRateB   <- dat1[ ,"surpProdRateB" ]
#
#  prodSSB     <- dat1[ ,"surpProdSSB" ]
#  prodRateSSB <- dat1[ ,"surpProdRateSSB" ]
#
#  It          <- dat1[, "It" ]
#  SSBt        <- dat1[ ,"SSBt" ]
#  totBt       <- dat1[ ,"totBt" ]
#
#  group       <- dat1[, "group" ]
#
#  groupList   <- unique( group )
#  nGroups     <- length( groupList )
#
#  if ( prodType=="total" )
#  {
#    biomass  <- totBt
#    surpProd <- prodB
#    prodRate <- prodRateB
#    xLab     <- "Total Biomass (000 t)"
#    yLab1    <- "Total Production (000 t)"
#    yLab2    <- "Total Production Rate"
#  }
#
#  if ( prodType=="totssb" )
#  {
#    biomass  <- SSBt
#    surpProd <- prodB
#    prodRate <- prodRateB
#    xLab     <- "Spawning Biomass (000 t)"
#    yLab1    <- "Total Production (000 t)"
#    yLab2    <- "Total Production Rate"
#  }
#
#  if ( prodType=="ssb" )
#  {
#    biomass  <- SSBt
#    surpProd <- prodSSB
#    prodRate <- prodRateSSB
#    xLab     <- "Spawning Biomass (000 t)"
#    yLab1    <- "SSB Production (000 t)"
#    yLab2    <- "SSB Production Rate"
#  }
#
#  if ( prodType=="survey" )
#  {
#    biomass  <- It
#    surpProd <- prodI
#    prodRate <- prodRateI
#    xLab     <- "Survey Biomass (000 t)"
#    yLab1    <- "Survey Production (000 t)"
#    yLab2    <- "Survey Production Rate"
#  }
#
#  par( oma=c(2,2.5,3,1), mar=c(2,2,3.5,1), mfrow=c(2,2) )
#
#  xAM1 <- data.frame( matrix( NA, nrow=2, ncol=11 ) )
#  xAM2 <- xAM1
#
#  # Model AM1.
#  cat( "\nResults for ", stockName," model AM1:\n" )
#  for ( i in 1:nGroups )
#  {
#    idx    <- group==groupList[i]
#    b      <- biomass[idx]
#    SP     <- surpProd[idx]
#    SPrate <- prodRate[idx]
#    yr     <- year[idx]
#  
#    # Survey survey 1951-1987.
#    if ( i==1 )
#    {
#      zone1 <- c( 1965:1969 )
#      if ( stockName=="CC8" || stockName=="CC67" )
#        zone1 <- c( 1964:1969 )
#      if ( stockName=="PRD" )
#         zone1 <- c( 1967:1972 )
#      if ( stockName=="SOG" )
#         zone1 <- c( 1966:1969 )
#      if ( stockName=="WCVI" )
#        zone1 <- c( 1966:1969 )
#      zone <- zone1    
#    }
#
#    # Diver survey 1988+
#    if ( i==2 )
#    {
#      zone2 <- c( 1988:2015 )
#  	  if ( stockName=="CC8" || stockName=="CC67" )
#         zone2 <- c( 2006:2011 )
#      if ( stockName=="HG" )
#         #zone <- c( 2000:2011 )
#         zone2 <- c( 2000:2008 )
#      if ( stockName=="WCVI" )
#         #zone <- c( 2005:2012 )
#         zone2 <- c( 2005:2012 )
#      zone <- zone2    
#    }
#
#    
#    #cat( "\nGroup = ", groupList[i],"\n" )
#    #cat( "\nMin Depletion = ", min( b / sbo ), "\n" )
#    #cat( "\nYear range of zone: ", zone, "\n" )
#
#    bTmp <- b[ match( zone, yr ) ]
#    {
#      zoneMin <- min( zone )
#      zoneMax <- max( zone )
#      nZone   <- length( zone )
#      dMin  <- min( bTmp / sbo )
#      yrMin <- zone[ bTmp==min(bTmp) ]
#      dMax  <- max( bTmp / sbo )
#      yrMax <- zone[ bTmp==max(bTmp) ]
#      dAvg  <- mean( bTmp / sbo )
#      B25   <- sbo * 0.25
#
#      
#      xAM1[i,] <- c( i, zoneMin, zoneMax, nZone, dMin, yrMin, dAvg, dMax, yrMax, B25, depCutoff )
#
#      #cat( "\nMin Depletion = ", dMin," in ", yrMin,"\n" )
#      #cat( "\nMax Depletion = ", dMax," in ", yrMax,"\n" )
#      #cat( "\nAvg Depletion = ", dAvg," over ", length(bTmp), "years\n" )
#      #cat( "\nBiomass at 0.25B0 = ", B25,"\n" )      
#      #cat( "\n")
#    }
#    #c( "Stock","Byr1",  "Byr2",  "nB", "DBmin","DBavg","DBmax",
#  	#                              "LPByr1","LPByr2","nPB","Dmin",  "Davg","Dmax",
#  	#                              "B25","D25","ltB25","ltB30","ltCut","Uavg" )
#    #resultAM1 <- data.frame( Stock=stockName, Byr1=min(zone1), Byr2=max(zone1),
#   # 	         nB=length(zone1), DBmin=min())
#
#    n      <- length( yr )
#
#    if ( xyScale )
#    {
#        #xLim  <- c( 0,max(b) )
#        xLim  <- c( 0, 2*fixedCutoff )
#        if ( stockName=="SOG" )
#          xLim <- c( 0,4*fixedCutoff )
#        yLim1 <- range( SP,na.rm=TRUE ) * 1.05
#        yLim2 <- range( SPrate, na.rm=TRUE ) *1.05
#    }
#    else
#    {
#      xLim   <- c( 0,max(biomass[idx], na.rm=TRUE) )
#      yLim1  <- range( surpProd, na.rm=TRUE )*1.05
#      yLim2  <- range( prodRate, na.rm=TRUE )*1.05
#    }
#
#    # Phase plot surplus production versus biomass.
#    plot( b, SP, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim1 )
#  
#    ## Draw arrows from point to point, not sure why "21", would have thought
#    #  should be 1988 when dive surveys started.
#
#    # Lay down depletions at 0.1, 0.25, 0.3 of B0.
#    deps <- c( 0.1, 0.25, 0.3 ) * sbo
#    abline( v=deps, lty=3, lwd=2, col=c( "red","blue","darkgreen") )
#    abline( v=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
#
#    lineCols <- rep( "black", length(b) )
#    s <- seq( length(b)-1 )
#    arrows( b[s], SP[s], b[s+1], SP[s+1], length=0.05, col=lineCols )
#  
#    # Lay down heat colors for order of points.
#    #colVec <- rev( heat.colors( n ) )
#    #colVec <- rev( gray.colors( n ) )
#    #points( b, SP, cex=cexSize, pch=21, bg=colVec )
#    points( b, SP, cex=cexSize, pch=21, bg="white" )
#
#    # Gray out the lower 20%.
#    idx <- b <= quantile( b, probs=0.2, na.rm=TRUE )
#    points( b[idx], SP[idx], cex=cexSize, pch=21, bg="gray" )
#
#    points( b[1], SP[1], cex=cexSize, pch=19, col="black" )
#    points( b[n-1], SP[n-1], cex=cexSize, pch=23, bg="black" )
#
#    text( b, SP, labels=yr, adj=1, cex=0.7, pos=3 )
#  
#    abline( h=0 )
#
#    axis( side=1 ) 
#    axis( side=2, las=2 )
#    #axis( side=3, labels=FALSE )
#    axis( side=4, labels=FALSE )
#
#    par( new="TRUE" )
#    plot( xLim/sbo, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
#    axis( side=3 )
#
#    box()
#
#    mtext( side=3, cex=1, line=2, groupList[i] )
#
#    mfg <- par( "mfg" )
#    if ( mfg[2]==1 )
#      mtext( side=2, cex=1.2, line=3, paste( "AM1",yLab1 ) )
#  }
#
#  #-----------------------------------------------------------------#
#  # Extract results for AM2.
#  # Extract AM1 quantities.
#  year        <- dat2[ ,"year" ]
#  sbo         <- repList2$sbo
#  fixedCutoff <- repList2$fixed_cutoff
#  depCutoff   <- fixedCutoff / sbo
#  
#  prodI       <- dat2[ ,"surpProdI" ]
#  prodRateI   <- dat2[ ,"surpProdRateI" ]
#
#  prodB       <- dat2[ ,"surpProdB" ]
#  prodRateB   <- dat2[ ,"surpProdRateB" ]
#
#  prodSSB     <- dat2[ ,"surpProdSSB" ]
#  prodRateSSB <- dat2[ ,"surpProdRateSSB" ]
#
#  It          <- dat2[, "It" ]
#  SSBt        <- dat2[ ,"SSBt" ]
#  totBt       <- dat2[ ,"totBt" ]
#
#  group       <- dat2[, "group" ]
#
#  groupList   <- unique( group )
#  nGroups     <- length( groupList )
#
#  if ( prodType=="total" )
#  {
#    biomass  <- totBt
#    surpProd <- prodB
#    prodRate <- prodRateB
#    xLab     <- "Total Biomass (000 t)"
#    yLab1    <- "Total Production (000 t)"
#    yLab2    <- "Total Production Rate"
#  }
#
#  if ( prodType=="totssb" )
#  {
#    biomass  <- SSBt
#    surpProd <- prodB
#    prodRate <- prodRateB
#    xLab     <- "Spawning Biomass (000 t)"
#    yLab1    <- "Total Production (000 t)"
#    yLab2    <- "Total Production Rate"
#  }
#
#  if ( prodType=="ssb" )
#  {
#    biomass  <- SSBt
#    surpProd <- prodSSB
#    prodRate <- prodRateSSB
#    xLab     <- "Spawning Biomass (000 t)"
#    yLab1    <- "SSB Production (000 t)"
#    yLab2    <- "SSB Production Rate"
#  }
#
#  if ( prodType=="survey" )
#  {
#    biomass  <- It
#    surpProd <- prodI
#    prodRate <- prodRateI
#    xLab     <- "Survey Biomass (000 t)"
#    yLab1    <- "Survey Production (000 t)"
#    yLab2    <- "Survey Production Rate"
#  }
#
#  # Model AM2.
#  cat( "\nResults for ", stockName," model AM2:\n" )
#  for ( i in 1:nGroups )
#  {
#    idx    <- group==groupList[i]
#    b      <- biomass[idx]
#    SP     <- surpProd[idx]
#    SPrate <- prodRate[idx]
#    yr     <- year[idx]
#
#    # Survey survey 1951-1987.
#    if ( i==1 )
#    {
#      zone1 <- c( 1965:1969 )
#      if ( stockName=="CC8" || stockName=="CC67" )
#        zone1 <- c( 1964:1969 )
#      if ( stockName=="PRD" )
#         zone1 <- c( 1967:1972 )
#      if ( stockName=="SOG" )
#         zone1 <- c( 1965:1970 )
#      if ( stockName=="WCVI" )
#        zone1 <- c( 1966:1969 )
#      zone <- zone1    
#    }
#
#    # Dive survey 1988+
#    if ( i==2 )
#    {
#      zone2 <- c( 1988:2015 )
#  	  if ( stockName=="CC8" || stockName=="CC67" )
#         zone2 <- c( 2006:2011 )
#      if ( stockName=="HG" )
#         # zone <- c( 2000:2011 )
#         zone2 <- c( 2000:2010 )
#      if ( stockName=="WCVI" )
#         # zone <- c( 2005:2012 )
#         zone2 <- c( 2004:2014 )
#      zone <- zone2    
#    }
#
#    
#    #cat( "\nMin Depletion = ", min( b / sbo ), "\n" )
#    #cat( "\nYear range of zone: ", zone, "\n" )
#
#    bTmp <- b[ match( zone, yr ) ]
#    {
#      zoneMin <- min( zone )
#      zoneMax <- max( zone )
#      nZone   <- length( zone )
#
#      dMin  <- min( bTmp / sbo )
#      yrMin <- zone[ bTmp==min(bTmp) ]
#      dMax  <- max( bTmp / sbo )
#      yrMax <- zone[ bTmp==max(bTmp) ]
#      dAvg  <- mean( bTmp / sbo )
#      B25   <- sbo * 0.25
#
#      xAM2[i,] <- c( i, zoneMin, zoneMax, nZone, dMin, yrMin, dAvg, dMax, yrMax, B25, depCutoff )
#
#      #cat( "\nMin Depletion = ", dMin," in ", yrMin,"\n" )
#      #cat( "\nMax Depletion = ", dMax," in ", yrMax,"\n" )
#      #cat( "\nAvg Depletion = ", dAvg," over ", length(bTmp), "years\n" )
#      #cat( "\nBiomass at 0.25B0 = ", B25,"\n" )      
#      #cat( "\n")
#    }
#
#    n      <- length( yr )
#
#    if ( xyScale )
#    {
#      #xLim  <- c( 0,max(b) )
#      #yLim1 <- range( SP ) * 1.05
#      #yLim2 <- range( SPrate ) *1.05
#    }
#    else
#    {
#      #xLim   <- c( 0,max(biomass) )
#      #yLim1  <- range( surpProd )*1.05
#      #yLim2  <- range( prodRate )*1.05
#    }
#
#    # Phase plot surplus production versus biomass.
#    plot( b, SP, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim1 )
#  
#    ## Draw arrows from point to point, not sure why "21", would have thought
#    #  should be 1988 when dive surveys started.
#
#    # Lay down depletions at 0.1, 0.25, 0.3 of B0.
#    deps <- c( 0.1, 0.25, 0.3 ) * sbo
#    abline( v=deps, lty=3, lwd=2, col=c( "red","blue","darkgreen") )
#    abline( v=fixedCutoff, col=.FIXCOL, lty=.FIXLTY, lwd=.FIXLWD )
#
#    lineCols <- rep( "black", length(b) )
#    s <- seq( length(b)-1 )
#    arrows( b[s], SP[s], b[s+1], SP[s+1], length=0.05, col=lineCols )
#  
#    # Lay down heat colors for order of points.
#    #colVec <- rev( heat.colors( n ) )
#    #colVec <- rev( gray.colors( n ) )
#    #points( b, SP, cex=cexSize, pch=21, bg=colVec )
#    points( b, SP, cex=cexSize, pch=21, bg="white" )
#
#    # Gray out the lower 20%.
#    idx <- b <= quantile( b, probs=0.2, na.rm=TRUE )
#    points( b[idx], SP[idx], cex=cexSize, pch=21, bg="gray" )
#
#    points( b[1], SP[1], cex=cexSize, pch=19, col="black" )
#    points( b[n-1], SP[n-1], cex=cexSize, pch=23, bg="black" )
#
#    text( b, SP, labels=yr, adj=1, cex=0.7, pos=3 )
#  
#    abline( h=0 )
#
#    axis( side=1 ) 
#    axis( side=2, las=2 )
#    #axis( side=3, labels=FALSE )
#    axis( side=4, labels=FALSE )
#
#    par( new="TRUE" )
#    plot( xLim/sbo, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
#    axis( side=3 )
#
#    box()
#
#    mtext( side=3, cex=1, line=2, groupList[i] )
#
#    mfg <- par( "mfg" )
#    if ( mfg[2]==1 )
#      mtext( side=2, cex=1.2, line=3, paste( "AM2",yLab1 ) )
#
#  }     # for i=1,nGroups
#
#  mtext( side=1, cex=1.2, line=0.5, outer=TRUE, xLab )
#
#  if ( stockName=="CC8" ) 
#    stockName <- "CC"
#
#  mtext( side=3, cex=1.3, line=1,   outer=TRUE, stockName )
#
#  par( mfrow=c(1,1) )
#
#  result <- data.frame( rbind( xAM1, xAM2 ) )
#  names( result ) <- c( "Group","zoneMin","zoneMax","nZone","dMin","yrMin","dAvg","dMax","yrMax","B25","dCutoff" )
#
#  return( result )
#}     # END function plotProdAM12
#
## Plot all the phase plots by stock and model.
#for ( i in 1:length( stockNames ) )
#{
#  tmpNames <- c( "Stock","Group","zoneMin","zoneMax","nZone","dMin","yrMin","dAvg","dMax","yrMax","B25" )
#  tmp <- data.frame( matrix( NA, nrow=length(stockNames), ncol=length(tmpNames) )	)
#  names( tmp ) <- tmpNames
#
#  sumTable <- as.list( c( 1:2 ) )
#  names( sumTable ) <- c( "AM1", "AM2")
#  sumTable$AM1 <- tmp
#  sumTable$AM2 <- tmp
#
#  fName <- paste( "phaseAM12",stockNames[i],".png", sep="" )
#  png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
#
#  result <- plotProdAM12( resultAM1[[i]], repListAM1[[i]],
#                resultAM2[[i]], repListAM2[[i]],
#      stockName=stockNames[i],
#      prodType=prodType, xyScale=TRUE )
#  print( result )
#
#  if ( iFooter )
#    mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
#
#  dev.off()
#
#  #sumTable$AM1[i,] <- c( stockNames[i], result[1,] )
#  #sumTable$AM2[i,] <- c( stockNames[i], result[2, ] )
#
#  #fName <- paste( "resultsAM1.csv", sep="" )
#  #write.csv( sumTable$AM1, file=fName, row.names=FALSE )  
#
#  #fName <- paste( "resultsAM2.csv", sep="" )
#  #write.csv( sumTable$AM2, file=fName, row.names=FALSE )   
#}
#
## Let's go get us some posterior values....
## JC says use *.mcst as the posterior SSBt values (no header names)
## Use iscam.mcmc for leading parameters (header names)
## There are 5,000 draws from a chain of 5M.
#
## Read AM2 files.
#caseFld  <- "AM2(historical)_5,000,000"
#baseName <- "Herring2016qBaseGeneric-pfcB"
#fileList <- list.files( caseFld )
#
## There are 2 Central Coast prefixes to account for the area adjustment cases,
## but they are not named uniquely so a few lines down that is fixed when
## mgmtAreas is patched and used to name the repList of *.rep file contents.
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI")
#
## Read in *.mcmc files for each stock.
#mcmcParsAM2 <- as.list( 1:length( mgmtAreas ) )
#mcmcSbtAM2  <- as.list( 1:length( mgmtAreas ) )
#for ( i in 1:length(repListAM2) )
#{
#  fName <- file.path( caseFld, fileList[i],
#             paste( mgmtAreas[i],baseName,".mcmc", sep="" ) )
#  mcmcParsAM2[[i]] <- read.table( fName, header=TRUE )
#  fName <- file.path( caseFld, fileList[i],
#             paste( mgmtAreas[i],baseName,".mcst", sep="" ) )
#  mcmcSbtAM2[[i]]  <- read.table( fName, header=FALSE )
#}
#
#mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI" )
#names( mcmcParsAM2 ) <- mgmtAreas
#names( mcmcSbtAM2 )  <- mgmtAreas
#
## which.min( abs(c(1:10)-median(c(1:10)))
#
## To find median depletion divide each row of mcmcSbtAM2 by
## sbo in same index value.
## sweep( sbt, 1, sbo, FUN="/" )
## then take median by column to get annual median depletion.
