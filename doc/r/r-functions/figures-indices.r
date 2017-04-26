make.index.fit.plot <- function(model,
                                start.yr,
                                end.yr,
                                ind,
                                ylim = c(0, 20),
                                ind.letter = NULL){
  ## Plot the index fit for an MPD
  ##
  ## ind - the index to plot

  ## Get index names in this model
  index.dat <- as.data.frame(model$dat$indices[[ind]])
  index.fit <- model$mpd$it_hat[ind,]
  index.fit <- index.fit[!is.na(index.fit)]
  xlim <- c(start.yr, end.yr)
  yrs <- index.dat$iyr
  index <- index.dat$it
  cv <- 1 / index.dat$wt

  ## Plot the fit first
  plot(yrs,
       index.fit,
       type = "l",
       lwd = 2,
       xlim = xlim,
       ylim = ylim,
       xlab = "",
       ylab = "",
       axes = FALSE)

  ## Then the points and arrows for the index inputs
  points(yrs,
         index,
         pch = 3,
         col = 1,
         lwd = 2)
  arrows(yrs,
         index + cv * index,
         yrs,
         index - cv * index,
         code = 3,
         angle = 90,
         length = 0.0,
         col = 1,
         lwd = 2)
  axis(1,
       at = seq(min(xlim), max(xlim)),
       labels = seq(min(xlim), max(xlim)))
  axis(2)
  box()
  mtext("Year", 1, line = 3)
  mtext("1,000 t", 2, line = 3)
  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
