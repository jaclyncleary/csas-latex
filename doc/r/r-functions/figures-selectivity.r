make.selex.comparison.plot <- function(models,
                                       gear.names,
                                       ind = NULL,
                                       areas = c("3C",
                                                 "3D",
                                                 "5A",
                                                 "5B",
                                                 "5C",
                                                 "5D",
                                                 "5E"),
                                       show.mat = FALSE,
                                       leg = NULL){
  ## Plot the selectivity for selected gear(s) in the model(s)
  ##
  ## gear.names - read in from the csv file in the data directory
  ## ind - the index number(s) for selectivity. If NULL and the
  ##  number of models is greater than 1, and error will occur. If
  ##  ind is a value, the models will be plotted against each other.
  ##  If ind is NULL and the models list is equal to 1, all gears
  ##  in the model will be plotted against each other.
  ## areas - the areas to include
  ## show.mat - plot the maturity as well
  ## leg - legend location, e.g. "bottomright"

  if(class(models) != model.lst.class){
    stop("The models argument is not of class '",
         model.lst.class, "'.")
  }

  if(length(models) > 1){
    if(is.null(ind)){
      stop("If ind is NULL, the length of the models list must be 1 to plot ",
           "all gears in that model.")
    }
    if(length(ind) > 1 | !is.numeric(ind)){
      stop("ind must be a single numeric value when plotting multiple models.")
    }
  }else{
    ## There is only one model
    if(is.null(ind)){
      ## Plot all gears given
      ind <- 1:length(gear.names)
    }
  }

  selex <- lapply(models,
                  function(x){
                    x$mpd$sel})
  est.phz <- lapply(models,
                    function(x){
                      x$ctl$sel[6,]
                    })
  est.phz <- lapply(1:length(models),
                    function(x){
                      sapply(1:length(ind),
                             function(y){
                               est.phz[[x]][y]})})
  if(is.data.frame(gear.names)){
    ## There are multiple indices and one model
    gear.names <- gear.names[ind, 2]
    ## Append " (Fixed)" to indices which have a negative phase
    gear.names <- sapply(1:length(gear.names),
                         function(y){
                           if(est.phz[[1]][y] < 0){
                             paste0(gear.names[y], " (Fixed)")
                           }else{
                             gear.names[y]
                           }})
  }else{
    ## There are multiple models and one index
    ## Append " (Fixed)" to indices which have a negative phase
    gear.names <- sapply(1:length(models),
                         function(x){
                           if(est.phz[[x]][1] < 0){
                             paste0(gear.names[x], " (Fixed)")
                           }else{
                             gear.names[x]
                           }})
  }
  age <- lapply(models,
                function(x){
                  x$mpd$age})[[1]]

  log.sel.dat <- lapply(models,
                        function(x){
                          x$mpd$log_sel})

  ## Make matrix for plotting
  sel.lst <-
    lapply(1:length(models),
           function(x){
             lapply(ind,
                    function(y){
                      log.sel.dat[[x]] <-
                        log.sel.dat[[x]][which(log.sel.dat[[x]][, 1] == y),]
                      yrs <- log.sel.dat[[x]][,3]
                      sel.dat <-
                        exp(log.sel.dat[[x]][, 4:ncol(log.sel.dat[[x]])])
                      ## end-year selectivity for the only block
                      sel.dat[nrow(sel.dat), ]
                    })
           })
  if(length(sel.lst) == 1){
    sel.mat <- do.call(cbind, sel.lst[[1]])
  }else{
    sel.mat <- do.call(cbind, sapply(sel.lst, function(x) x[1]))
  }

  col <- seq(1, ncol(sel.mat))
  lty <- rep(1, ncol(sel.mat))
  lwd <- rep(2, ncol(sel.mat))

  matplot(age,
          sel.mat,
          type = "l",
          lwd = lwd,
          col = col,
          lty = lty,
          las = 1,
          xlim = c(1, max(age)),
          ylim = c(0, 1.1),
          ylab = "Selectivity",
          xlab = "Age")

  if(show.mat){
    warning("Not yet implemented.")
  }
  if(!is.null(leg)){
    legend(leg,
           legend = gear.names,
           col = col,
           lty = lty,
           lwd = lwd)
  }
}

