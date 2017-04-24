make.age.comp.plot <- function(model,
                               ind,
                               type = 1,
                               add = TRUE,
                               ind.letter = NULL){
  ## Plot the age comp data, two-paned if the model is two-sex
  ##
  ## ind - the index of the gear to plot as found in the data file
  ## type - 1 = data, 2 = fit, 3 = residuals
  ## add - if TRUE, par will not be restored on exit

  if(!add){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
  }

  n.sex <- model$dat$num.sex
  n.gears <- model$dat$num.age.gears
  n.gear.obs <- model$dat$age.gears.n
  age.comp.flags <- model$dat$age.comp.flag
  if(model$dat$has.gear.names){
    gear.nm <- model$dat$gear.names[ind]
  }else{
    gear.nm <- paste0("Gear ", ind)
  }

  comp.dat <- as.data.frame(model$mpd$d3_A)
  gears <- unique(comp.dat[, 2])
  fit.dat <- as.data.frame(model$mpd$A_hat)
  resid.dat <- as.data.frame(model$mpd$A_nu)
  if(!(ind %in% gears)){
    warning("The index you supplied was not found in the list of gears. ",
            "The age comp plot will not be made.")
    return(invisible())
  }
  gear.ind <- which(gears == ind)
  if(length(n.gear.obs[[gear.ind]]) == 0){
    warning("The number of samples for each year are missing from the data ",
            "file. They should appear after a comment character at the end of ",
            "each line of age/length comp data. The number of samples will ",
            "not be shown on the plot.")
  }
  s.age <- model$mpd$n_A_sage[gear.ind]
  n.age <- model$mpd$n_A_nage[gear.ind]
  ages <- s.age:n.age
  n.ages <- length(s.age:n.age)
  flag <- age.comp.flags[gear.ind]
  if(flag){
    y.lab="Age"
  }else{
    y.lab="Length"
  }

  ## The fit.dat and resid.dat have unlabelled rows, which need to be extracted
  ##  correctly based on the values in the comp.dat data.frame
  which.rows <- which(comp.dat[,2] == ind)
  comp.dat <- comp.dat[which.rows ,]
  yrs <- comp.dat[, 1]
  ## Column 5 holds the sex. 0 = combined, 1 = male, 2 = female
  ## They will either all be 0, or rows combined with 1 and 2
  if(n.sex == 1){
  }else{
  }
  fit.dat <- fit.dat[which.rows,]
  resid.dat <- resid.dat[which.rows,]
  ## Remove the information columns, leaving only the comps
  comp.dat <- as.matrix(comp.dat[, -seq(1, 5)])
  obs.prop <- prop.table(comp.dat, 1)

  if(type == 1){
    plotBubbles(t(obs.prop),
                xval = yrs,
                yval = ages,
                size = 0.1,
                powr = 0.5,
                xlab = "Year",
                ylab = y.lab,
                las = 1,
                cex = 1.25,
                axes = FALSE)
    axis(1, at = yrs, labels = yrs)
  }else if(type == 2){
    max.y <- max(fit.dat, obs.prop)
    n.side <- get.rows.cols(length(yrs))
    par(mfrow = n.side,
        oma = c(2, 3, 1, 1),
        mai = c(0.2, 0.4, 0.3, 0.2))
    for(yr in 1:length(yrs)){
      year <- yrs[yr]
      obs <- obs.prop[yr,]
      est <- fit.dat[yr,]
      plot(ages,
           obs,
           type = "h",
           xlab = "",
           ylab = "",
           main = year,
           las = 1,
           ylim = c(0, max.y))
      lines(ages, est, lty = 1, lwd = 2, col = 2)
    }
    par(old.par)
  }else if(type == 3){
    plotBubbles(t(resid.dat),
                xval = yrs,
                yval = ages,
                size = 0.1,
                powr = 0.5,
                xlab = "Year",
                ylab = y.lab,
                las = 1,
                cex = 1.25,
                axes = FALSE)
    axis(1, at = yrs, labels = yrs)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

get.rows.cols <- function(num){
  ## Returns a vector of length 2 representing the number of rows and columns
  ##  to use to pack a plot in a grid.
  if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}
