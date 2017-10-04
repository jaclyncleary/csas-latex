make.recr.mcmc.plot <- function(models,
                                model.names = NULL,
                                type = 1,
                                ylim,
                                offset = 0.1,
                                append.base.txt = NULL,
                                show.median = FALSE,
                                show.mean = FALSE,
                                ind.letter = NULL,
                                leg = NULL,
                                ... ){
  ## Plot the recruitment or deviations with credibility intervals for
  ##  the mcmc case of the models.
  ##
  ## type - 1 = recruitment estimates, any other = recruitment deviation
  ##  estimates
  ## offset - the amount on the x-axis to offset each point and line for
  ##  multiple models
  ## append.base.txt - text to append to the name of the first model
  ## show.median - for recruitment plot only
  ## show.mean - for recruitment plot only

  rt <- lapply(models,
               function(x){
                 if(type == 1){
                   x$mcmccalcs$recr.quants
                 }else{
                   x$mcmccalcs$recr.devs.quants
                 }})
  yrs <- lapply(rt,
                function(x){
                  as.numeric(colnames(x))})
  xlim <- lapply(1:length(yrs),
                 function(x){
                   c(min(yrs[[x]]), max(yrs[[x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  plot.new()
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")
  lapply(1:length(yrs),
         function(x){
           points(yrs[[x]] - (x - 1) * offset,
                  rt[[x]][2,],
                  type = "p",
                  pch = 20,
                  col = x,
                  xlab = "",
                  ylab = "",
                  las = 1,
                  xlim = xlim,
                  ylim = ylim)})
  lapply(1:length(yrs),
         function(x){
           arrows(yrs[[x]] - (x - 1) * offset,
                  rt[[x]][1,],
                  yrs[[x]] - (x - 1) * offset,
                  rt[[x]][3,],
                  col = x,
                  code = 3,
                  angle = 90,
                  length = 0.02)})

  if(type == 1 & show.median){
    abline(h = median(as.matrix(rt[[1]])),
           col = "green",
           lty = 1)
  }
  if(type == 1 & show.mean){
    abline(h = mean(as.matrix(rt[[1]])),
           col = "red",
           lty = 1)
  }
  axis(1, at = yrs[[1]], labels = yrs[[1]])
  axis(2)
  box()
  mtext("Year", 1, line = 3)
  if(type == 1){
    mtext("Recruitment (millions)", 2, line = 3)
  }else{
    mtext("Log recruitment deviations", 2, line = 3)
  }

  if(!is.null(model.names) & !is.null(leg)){
    if(!is.null(append.base.txt)){
      model.names[[1]] <- paste0(model.names[[1]],
                                 append.base.txt)
    }
    legend(leg,
           model.names,
           bg = "transparent",
           col = 1:length(models),
           lty = 1,
           lwd = 2)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

make.recr.retro.mpd.plot <- function(base.model,
                                     models,
                                     model.names = NULL,
                                     ylim,
                                     offset = 0.1,
                                     ind.letter = NULL,
                                     leg = NULL,
                                     color.brew.class = "Paired",
                                     ...
                                     ){
  ## Plot the recruitment for the mpd case of the models
  ##
  ## offset - the amount on the x-axis to offset each point and line for
  ##  multiple models
  ## append.base.txt - text to append to the name of the first model
  ## show.bo.line - show the reference lines 0.2 and 0.4bo

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  rt <- lapply(models,
                function(x){
                  x[[1]]$mpd$rt})

  yrs <- lapply(models,
                function(x){
                  (x[[1]]$mpd$syr +2):x[[1]]$mpd$nyr})
  xlim <- lapply(1:length(yrs),
                 function(x){
                   c(min(yrs[[x]]), max(yrs[[x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  if(is.null(dev.list())){
    ## If layout() is used outside this function,
    ##  it calls plot.new and will mess up the figures
    ##  if we call it again
    plot.new()
  }
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")

  base.yrs <- (base.model$mpd$syr + 2):base.model$mpd$nyr
  base.rt <- base.model$mpd$rt

  plot(base.yrs,
       base.rt,
       col = 1,
       lwd = 3,
       las = 1,
       lty = 1,
       xlim = xlim,
       ylim = ylim,
       xlab = "",
       ylab = "",
       type = "l",
       ...)
  cols <- brewer.pal(length(model.names) - 1, color.brew.class)
  lapply(1:length(yrs),
         function(x){
           lines(yrs[[x]],
                 rt[[x]],
                 xlab = "",
                 ylab = "",
                 col = cols[x],
                 las = 1,
                 lwd = 2,
                 lty = 2,
                 xlim = xlim,
                 ylim = ylim,
                 ...)})

  mtext("Year", 1, line = 3)
  mtext("Recruitment (millions)", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    legend(leg,
           model.names,
           bg = "transparent",
           col = c(1, cols),
           lty = c(1, rep(2, length(model.names) - 1)),
           lwd = 2)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

