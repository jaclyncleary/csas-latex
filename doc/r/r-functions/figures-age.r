make.age.comp.estimates.plot <- function(model,
                                         which.gear,
                                         fg = gray(level = 0.1, alpha = 0.5),
                                         bg = gray(level = 0.5, alpha = 0.5),
                                         inches = 0.12){

  ## ahat <- t(model$mpd$ahat[[which.gear]])
  ## ahat is a matrix which needs to be broken up into smaller
  ##  matrices, one for each gear
  ahat <- model$mpd$A_hat
  nagv <- model$dat$num.age.gears.vec
  sage <- model$mpd$n_A_sage[1]
  nage <- model$mpd$n_A_nage[1]
  ## Need the age comp data for the years
  age.comps <- model$dat$age.comps

  a.props <- list()
  ind <- 1
  for(i in 1:length(nagv)){
    yrs <- as.data.frame(age.comps[[i]])$year
    a.props[[i]] <- ahat[ind:(ind + length(yrs) - 1),]
    rownames(a.props[[i]]) <- yrs
    colnames(a.props[[i]]) <- sage:nage
    ind <- ind + length(yrs)
  }
  dat <- a.props[[which.gear]]

  x <- data.frame(expand.grid(as.numeric(rownames(dat)),
                              as.numeric(colnames(dat))),
                  prop = as.vector(dat))
  yrs <- as.numeric(rownames(dat))
  names(x) <- c("yr", "age", "prop")
  max.prop <- max(dat)
  symbols(c(x[,1], -1),
          c(x[,2], -1),
          circles = sqrt(c(x[,3], max.prop)),
          inches = inches,
          ylim = c(sage, nage),
          xlim = c(min(yrs), max(yrs)),
          xlab = "",
          ylab = "Age",
          xaxt = "n",
          fg = fg,
          bg = bg)
  axis(1, yrs)
  axis(4)
}

make.age.comp.data.plot <- function(model,
                                    which.gear){

  a <- model$dat$age.comps[[which.gear]]
  yrs <- a[,1]
  a <- a[,-c(1, 2, 3, 4, 5)]
  plotBubbles(t(a),
              xval = yrs,
              yval = colnames(a),
              size = 0.1,
              powr = 0.5,
              xlab = "Year",
              ylab = "Age",
              cex = 0.75)
##              axes = FALSE)

}
