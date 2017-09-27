make.fishing.mort.mcmc.plot <- function(models,
                                        model.names = NULL,
                                        which.gear = 1,
                                        ylim,
                                        opacity = 75,
                                        ind.letter = NULL,
                                        leg = NULL,
                                        ...
                                        ){
  ## Plot the biomass with credibility intervals for the mcmc
  ##  case of the models

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  f.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$f.mort.quants[[which.gear]]})
  yrs <- lapply(f.quants,
                function(x){
                  as.numeric(colnames(x))})
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

  lapply(1:length(yrs),
         function(x){
           draw.envelope(yrs[[x]],
                         f.quants[[x]],
                         xlab = "",
                         ylab = "",
                         col = x,
                         las = 1,
                         xlim = xlim,
                         ylim = ylim,
                         opacity = opacity,
                         first = ifelse(x == 1, TRUE, FALSE),
                         ...)})

  mtext("Year", 1, line = 3)
  mtext("Fishing mortality", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
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

##                                         model,
##                                         ylim,
##                                         opacity = 75,
##                                         col = "black",
##                                         ind.letter = NULL,
##                                         ...
##                                         ){
##   ## Plot the fishing mortality with credibility intervals for the mcmc
##   ##  case of the model
##   ##
##   ## y.max - upper limit for the y axis
##   ## opacity - how opaque the envelope is

##   if(class(model) == model.lst.class){
##     model <- model[[1]]
##     if(class(model) != model.class){
##       stop("The structure of the model list is incorrect.")
##     }
##   }

##   par(mar = c(5.1, 5.1, 4.1, 3.1))

##   f.mort <- model$mcmccalcs$f.mort.quants[[1]]
##   yrs <- as.numeric(colnames(f.mort))
##   xlim <- c(min(yrs), max(yrs))

##   draw.envelope(yrs,
##                 f.mort,
##                 ylab = "F",
##                 xlab = "Year",
##                 col = col,
##                 las = 1,
##                 xlim = xlim,
##                 ylim = ylim,
##                 opacity = opacity,
##                 first = TRUE,
##                 ...)

##   if(!is.null(ind.letter)){
##     panel.letter(ind.letter)
##   }
## }
