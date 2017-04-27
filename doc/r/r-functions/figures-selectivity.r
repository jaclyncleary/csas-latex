make.selex.comparison.plot <- function(model,
                                       gear.names,
                                       areas = c("3C",
                                                 "3D",
                                                 "5A",
                                                 "5B",
                                                 "5C",
                                                 "5D",
                                                 "5E"),
                                       show.mat = FALSE,
                                       leg = NULL){
  ## Plot the selectivity for all gears in the model
  ##
  ## gear.names - read in from the csv file in the data directory
  ## areas - the areas to include
  ## show.mat - plot the maturity as well
  ## leg - legend location, e.g. "bottomright"

  selex <- model$mpd$sel
  est.phz <- model$ctl$sel[6,]
  gear.names <- gear.names[,2]
  gear.names[est.phz < 0] <- paste0(gear.names[est.phz < 0], " (Fixed)")
  age <- model$mpd$age

  log.sel.dat <- model$mpd$log_sel
  ## Make matrix for plotting
  sel.lst <- lapply(1:nrow(selex),
                    function(x){
                      log.sel.dat <- log.sel.dat[which(log.sel.dat[,1] == x),]
                      yrs <- log.sel.dat[,3]
                      sel.dat <- exp(log.sel.dat[, 4:ncol(log.sel.dat)])
                      ## end-year selectivity for the only block
                      sel.dat <- sel.dat[nrow(sel.dat), ]
                    })
  sel.mat <- do.call(cbind, sel.lst)

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
