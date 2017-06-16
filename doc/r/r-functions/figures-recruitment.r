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

  if(class(models) != model.lst.class){
    stop("The models argument is not of class '",
         model.lst.class, "'.")
  }

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
