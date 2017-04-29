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

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

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

make.biomass.mcmc.plot <- function(models,
                                   model.names = NULL,
                                   y.max,
                                   opacity = 75,
                                   bo.offset = 0.1,
                                   col = "black",
                                   show.bmsy.line = FALSE,
                                   show.bo.line = FALSE,
                                   ind.letter = NULL,
                                   leg = NULL,
                                   ...
                                   ){
  ## Plot the biomass with credibility intervals for the mcmc
  ##  case of the models
  ##
  ## y.max - upper limit for the y axis
  ## opacity - how opaque the envelope is
  ## bo.offset - offset to the left for the bo point an bars
  ## show.bmsy.line - show the reference lines 0.4 and 0.8bmsy
  ## show.bo.line - show the reference lines 0.2 and 0.4bo

  if(class(models) != model.lst.class){
    stop("The models argument is not of class '", models.lst.class, "'.")
  }

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  sbt.quants <- lapply(models,
                       function(x){
                         x$mcmccalcs$sbt.quants})
  r.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$r.quants})
  sbo.raw <- lapply(r.quants,
                    function(x){
                      x[rownames(x) == "bo", ]})
  sbo <- lapply(sbo.raw,
                function(x){
                  as.numeric(x[,2:4])})
  yrs <- lapply(sbt.quants,
                function(x){
                  as.numeric(colnames(x))})
  ## Plot the fit first
  plot.new()
  plot.window(xlim = c(min(yrs[[1]]), max(yrs[[1]])),
              ylim = c(0, y.max),
              xlab = "",
              ylab = "")

  p.env <- lapply(1:length(yrs),
                  function(x){
                      draw.envelope(yrs[[x]],
                                    sbt.quants[[x]],
                                    xlab = "",
                                    ylab = "",
                                    col = x,
                                    las = 1,
                                    y.max = y.max,
                                    opacity = opacity,
                                    first = ifelse(x == 1, TRUE, FALSE),
                                    ...)})
  ## Add sbo points and ci bars
  p.pts <- lapply(1:length(yrs),
                  function(x){
                    points(yrs[[x]][1] - bo.offset + 0.2 * x,
                           sbo[[x]][2],
                           pch = 19,
                           col = x)
                    arrows(yrs[[x]][1] - bo.offset + 0.2 * x,
                           sbo[[x]][1],
                           yrs[[x]][1] - bo.offset + 0.2 * x,
                           sbo[[x]][3],
                           lwd = 2,
                           code = 0,
                           col = x)})

  if(show.bo.line){
    abline(h = 0.2 * sbo[[1]][2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.2B"[0]),
          4,
          at = 0.2 * sbo[[1]][2],
          col = "red",
          las = 1)
    abline(h = 0.4 * sbo[[1]][2],
           col = "green",
           lty = 1,
           lwd = 2)
    mtext(expression("0.4B"[0]),
          4,
          at = 0.4 * sbo[[1]][2],
          col = "green",
          las = 1)
  }
  if(show.bmsy.line){
    sbmsy.raw <- r.quants[[1]][rownames(r.quants[[1]]) == "bmsy", ]
    sbmsy <- as.numeric(sbmsy.raw[2:4])
    browser()
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
  mtext("Year", 1, line = 3)
  mtext("Biomass (1000 mt)", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    legend(leg,
           model.names,
           col = 1:length(models),
           lty = 1,
           lwd = 2)
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

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

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
