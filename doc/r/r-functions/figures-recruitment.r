make.recr.mcmc.plot <- function(model,
                                y.max,
                                col = "black",
                                ind.letter = NULL,
                                ...
                                ){
  ## Plot the recruitment with credibility intervals for the mcmc
  ##  case of the model.
  ##
  ## y.max - upper limit for the y axis

  rt <- model$mcmccalcs$recr.quants
  yrs <- as.numeric(colnames(rt))

  plot(yrs,
       rt[2,],
       type = "p",
       pch = 20,
       col = col,
       ylim = c(0, y.max),
       xlim = c(min(yrs), max(yrs)),
       xlab = "Year",
       ylab = "Recruitment (millions)",
       las = 1)

  arrows(yrs,
         rt[1,],
         yrs,
         rt[3,],
         col = col,
         code = 3,
         angle = 90,
         length = 0.05)

  abline(h = median(as.matrix(rt)),
         col = "green",
         lty = 1)
  abline(h = mean(as.matrix(rt)),
         col = "red",
         lty = 1)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

make.recr.devs.mcmc.plot <- function(model,
                                     y.min = 0,
                                     y.max,
                                     col = "black",
                                     ind.letter = NULL,
                                     ...
                                     ){
  ## Plot the recruitment deviations with credibility intervals for the mcmc
  ##  case of the model.
  ##
  ## y.min - lower limit for the y axis
  ## y.max - upper limit for the y axis

  rdev <- model$mcmccalcs$recr.devs.quants
  yrs <- as.numeric(colnames(rdev))

  plot(yrs,
       rdev[2,],
       type = "p",
       pch = 20,
       col = col,
       ylim = c(y.min, y.max),
       xlim = c(min(yrs), max(yrs)),
       xlab = "Year",
       ylab = "Log recruitment deviations",
       las = 1)

  arrows(yrs,
         rdev[1,],
         yrs,
         rdev[3,],
         col = col,
         code = 3,
         angle = 90,
         length = 0.05)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
