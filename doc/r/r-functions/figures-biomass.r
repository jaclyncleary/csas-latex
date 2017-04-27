make.vuln.biomass.mcmc.plot <- function(model,
                                        y.max,
                                        opacity = 75,
                                        col = "black",
                                        show.spawn.bio = FALSE,
                                        spawn.bio.col = "red",
                                        leg = "topleft",
                                        ind.letter = NULL,
                                        ...
                                        ){
  ## Plot the vulnerable biomass with credibility intervals for the mcmc
  ##  case of the model.
  ##
  ## y.max - upper limit for the y axis
  ## opacity - how opaque the envelope is
  ## show.spawn.bio - add the spawning biomass to the plot
  ## leg - legend location (only used if show.spawn.bio is TRUE

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  vbt <- model$mcmccalcs$vuln.quants[[1]]
  yrs <- as.numeric(colnames(vbt))
  if(show.spawn.bio){
    sbt <- model$mcmccalcs$sbt.quants
    draw.envelope(yrs,
                  sbt,
                  ylab = "Biomass (1000 mt)",
                  xlab = "Year",
                  col = spawn.bio.col,
                  las = 1,
                  y.max = y.max,
                  opacity = opacity,
                  first = TRUE,
                  ...)
    legend(leg,
           legend = c("Vulnerable Biomass",
                      "Spawning Biomass"),
           col = c(col,
                   spawn.bio.col),
           lty = 1,
           lwd = 2)
  }
  draw.envelope(yrs,
                vbt,
                ylab = "Biomass (1000 mt)",
                xlab = "Year",
                col = col,
                las = 1,
                y.max = y.max,
                opacity = opacity,
                first = !show.spawn.bio,
                ...)
}

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

make.rel.biomass.mcmc.plot <- function(model,
                                       opacity = 75,
                                       col = "black",
                                       ind.letter = NULL,
                                       ...
                                       ){
  ## Plot the relative biomass with credibility intervals for the mcmc
  ##  case of the model
  ##
  ## opacity - how opaque the envelope is

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  depl <- model$mcmccalcs$depl.quants
  yrs <- as.numeric(colnames(depl))

  draw.envelope(yrs,
                depl,
                ylab = "Relative spawning biomass",
                xlab = "Year",
                col = col,
                las = 1,
                y.max = 1,
                opacity = opacity,
                first = TRUE,
                ...)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
