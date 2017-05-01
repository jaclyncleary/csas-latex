make.fishing.mort.mcmc.plot <- function(model,
                                        ylim,
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

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  f.mort <- model$mcmccalcs$f.mort.quants[[1]]
  yrs <- as.numeric(colnames(f.mort))
  xlim <- c(min(yrs), max(yrs))

  draw.envelope(yrs,
                f.mort,
                ylab = "F",
                xlab = "Year",
                col = col,
                las = 1,
                xlim = xlim,
                ylim = ylim,
                opacity = opacity,
                first = TRUE,
                ...)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
