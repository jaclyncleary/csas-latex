make.reference.points.plot <- function(models,
                                       ref.pts = c("bo",
                                                   "bmsy",
                                                   "msy",
                                                   "umsy")){
  ## Plot select reference points
  ##
  ## ref.pts - the reference points to plot. These are the column names of
  ##  the r.dat data frame generated in calc.mcmc()

  ## if(class(model) == model.lst.class){
  ##   model <- model[[1]]
  ##   if(class(model) != model.class){
  ##     stop("The structure of the model list is incorrect.")
  ##   }
  ## }

  r.dat <- lapply(models,
                  function(x){
                    x$mcmccalcs$r.dat
                  })
  r.inc <- lapply(r.dat,
                  function(x){
                    x[, colnames(x) %in% ref.pts]
                  })

  n.side <- get.rows.cols(length(ref.pts))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  lapply(1:length(ref.pts),
         function(x){
           ## vec <- r.inc[, x]
           vecs <- lapply(1:length(r.inc),
                          function(y){
                            r.inc[[y]][, x]
                          })
           ## Get fancy latex name for the reference point
           vec.nm <- colnames(r.inc[[1]])[x]
           vec.nm <- get.latex.name(vec.nm)
           y.max <- max(sapply(vecs,
                               max))
           boxplot(vecs,
                   pch = 20,
                   ylim = c(0, y.max),
                   main = vec.nm,
                   border = 1:length(models))
         })
  invisible()
}
