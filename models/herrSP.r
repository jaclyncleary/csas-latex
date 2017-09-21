# Source functions needed to read iSCAM files.
source( "read.admb.r" )

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
#    It    <- obj$it_hat
    It    <- obj$d3_survey_data[, "V2"]
    # obj$it has an endline at what corresponds to 1987 so that the intended
    # vector gets read in as 2 row by 37 matrix.
    # as.vector rearranges by row, so order of years will be incorrect.
#    It    <- as.vector( t( It ) )[1:nYrs]
    
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
  yLim  <- c( 0, max(SSBt,Ct,na.rm=TRUE) )
  
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
      paste( stockName,": ",modType, sep="" ) ) 
  #paste( stockName,": AM",modType,"(",qList[2],")", sep="" ) )
  
  par( mfrow=c(1,1) )
  return( invisible() )
}     # END function plotSAR

# File name (*.rep)
baseName <- "iscam"

##### Start a loop over models (cases) #####
for( m in 1:length(mNames) ) {
  
  # Get the model name
  caseFld  <- mNames[m]
  
  # There are 2 Central Coast prefixes to account for the area adjustment cases,
  # but they are not named uniquely so a few lines down that is fixed when
  # mgmtAreas is patched and used to name the repList of *.rep file contents.
  mgmtAreas <- c( "CC","HG","PRD","SOG","WCVI")
  fileList <- mgmtAreas
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
  
  names( repList ) <- mgmtAreas
  
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
    
    # Figures for SAR.
    fName <- file.path( stockNames[i], 
        paste( "Production",caseFld,".png", sep="" ) )
    png( file=fName, height=yHeight, width=xWidth, pointsize=ptSize )
    plotSAR( result[[i]], repList[[i]], stockName=stockNames[i],
        modType=caseFld, prodType=prodType, byGroup=byGrp, xyScale=xyScale )
    
    if ( iFooter )
      mtext( side=1, adj=1, line=1, cex=0.7, outer=TRUE, repFiles[i] )
    dev.off()   
    
  }
  
  ##### End m loop over models (cases) #####
}
