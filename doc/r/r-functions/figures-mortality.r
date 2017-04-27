make.fishing.mort.mcmc.plot <- function(model,
                                        y.max,
                                        opacity = 75,
                                        col = "black",
                                        ind.letter = NULL,
                                        ...
                                        ){
  ## Plot the fishing mortality with credibility intervals for the mcmc
  ##  case of the model
  ##
  ## y.max - upper limit for the y axis
  ## opacity - how opaque the envelope is

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  f.mort <- model$mcmccalcs$f.mort.quants[[1]]
  yrs <- as.numeric(colnames(f.mort))

  draw.envelope(yrs,
                f.mort,
                ylab = "F",
                xlab = "Year",
                col = col,
                las = 1,
                y.max = y.max,
                opacity = opacity,
                first = TRUE,
                ...)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
