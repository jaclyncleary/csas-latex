make.reference.points.plot <- function(model,
                                       ref.pts = c("bo",
                                                   "bmsy",
                                                   "msy",
                                                   "umsy")){
  ## Plot select reference points
  ##
  ## ref.pts - the reference points to plot. These are the column names of
  ##  the r.dat data frame generated in calc.mcmc()

  r.dat <- model$mcmccalcs$r.dat
  r.inc <- r.dat[, colnames(r.dat) %in% ref.pts]

  n.side <- get.rows.cols(length(ref.pts))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  lapply(1:length(ref.pts),
         function(x){
           vec <- r.inc[, x]
           vec.nm <- colnames(r.inc)[x]
           vec.nm <- get.latex.name(vec.nm)
           y.max <- max(vec)
           boxplot(vec,
                   pch = 20,
                   ylim = c(0, y.max),
                   main = vec.nm)})
}
