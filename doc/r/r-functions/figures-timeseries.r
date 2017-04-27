make.biomass.mcmc.plot <- function(model,
                                   y.max,
                                   opacity = 75,
                                   bo.offset = 0.1,
                                   col = "black",
                                   show.bmsy.line = FALSE,
                                   show.bo.line = FALSE,
                                   ind.letter = NULL,
                                   ...
                                   ){
  ## Plot the biomass with credibility intervals for the mcmc
  ##  case of the model
  ##
  ## y.max - upper limit for the y axis
  ## opacity - how opaque the envelope is
  ## bo.offset - offset to the left for the bo point an bars
  ## show.bmsy.line - show the reference lines 0.4 and 0.8bmsy
  ## show.bo.line - show the reference lines 0.2 and 0.4bo

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  sbt <- model$mcmccalcs$sbt.quants
  r.quants <- model$mcmccalcs$r.quants
  sbo.raw <- r.quants[rownames(r.quants) == "bo", ]
  sbo <- as.numeric(sbo.raw[,2:4])
  yrs <- as.numeric(colnames(sbt))
  draw.envelope(yrs,
                sbt,
                ylab = "Biomass (1000 mt)",
                xlab = "Year",
                col = col,
                las = 1,
                y.max = y.max,
                opacity = opacity,
                first = TRUE,
                ...)
  ## Add sbo point and ci bars
  points(yrs[1] - bo.offset,
         sbo[2],
         pch = 19,
         col = col)
  arrows(yrs[1] - bo.offset,
         sbo[1],
         yrs[1] - bo.offset,
         sbo[3],
         lwd = 2,
         code = 0,
         col = col)

  if(show.bo.line){
    abline(h = 0.2 * sbo[2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.2B"[0]),
          4,
          at = 0.2 * sbo[2],
          col = "red",
          las = 1)
    abline(h = 0.4 * sbo[2],
           col = "green",
           lty = 1,
           lwd = 2)
    mtext(expression("0.4B"[0]),
          4,
          at = 0.4 * sbo[2],
          col = "green",
          las = 1)
  }
  if(show.bmsy.line){
    sbmsy.raw <- r.quants[rownames(r.quants) == "bmsy", ]
    sbmsy <- as.numeric(sbmsy.raw[,2:4])
    abline(h = 0.4 * sbmsy[2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.4B"[MSY]),
          4,
          at = 0.4 * sbmsy[2],
          col = "red",
          las = 1)
    abline(h = 0.8 * sbmsy[2],
           col = "green",
           lty = 1,
           lwd = 2)
    mtext(expression("0.8B"[MSY]),
          4,
          at = 0.8 * sbmsy[2],
          col = "green",
          las = 1)
  }
  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

draw.envelope <- function(yrs,
                          quants,
                          col = "black",
                          y.min = 0,
                          y.max,
                          first,
                          opacity = 75,
                          ...){
  ## Draw a time series envelope on a device on which plot.new has already
  ##  been called.
  ## Assumptions: quants is a 3-row matrix, where the middle row is the
  ##  median and the other two are the lower and upper values for some
  ##  confidence interval.
  ## y.min - lower limit for the y-axis
  ## y.max - upper limit for the y-axis
  ## first - boolean, if TRUE, plot will be called. If FALSE, lines will be
  ##  called.
  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs,
         median,
         type = "l",
         col = col,
         lty = 1,
         lwd = 2,
         ylim = c(y.min, y.max), ...)
    shade <- get.shade(col, opacity)
    poly.yrs <- c(yrs, rev(yrs))
    poly.ci    <- c(lower, rev(upper))
    polygon(poly.yrs, poly.ci, col = shade)
  }else{
    lines(yrs,
          median,
          type = "l",
          col = col,
          lty = 1,
          lwd = 2,
          ylim = c(y.min, y.max),
          ...)
    ## Upper and lower part of CI
    lines(yrs,
          lower,
          col = color,
          lty = 5,
          lwd = 1)
    lines(yrs,
          upper,
          col = color,
          lty = 5,
          lwd = 1)
  }
}
