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

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

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
    mtext("Age", side = 1, line = 0, outer = TRUE)
    mtext("Proportion", side = 2, line = 1, outer = TRUE)
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

make.length.plot <-
  function(dat,
           start.yr,
           sex = 1,
           plot.subfleet = TRUE,
           y.lim = c(20, 90),
           subfleet.vrn = c(103548, ## Viking Enterprise FOS
                            109710, ## Northern Alliance FOS
                            103808, ## Osprey #1         FOS
                            120250, ## Raw Spirit        FOS
                            568,  ## Viking Enterprise GFBIO
                            592,  ## Northern Alliance GFBIO
                            569,  ## Osprey #1         GFBIO
                            595), ## Raw Spirit        GFBIO
           add = TRUE,
           ind.letter = NULL){
  ## Plot the length data for certain vessels, either the ones listed in
  ##  subfleet.vrn or the ones not in subfleet.vrn, depending on
  ##  plot.subfleet.
  ##
  ## start.yr - year to start the plot
  ## sex - 1 = female, 2 = male
  ## plot.subfleet - if TRUE, plot length distributions for the vessels listed
  ##  in subfleet.vrn. If FALSE, plot all other vessels not in subfleet.vrn
  ## subfleet.vrn - a vector of vessel IDs for vessels to segregate

  if(!add){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
  }

  d <- dat[dat$Year >= start.yr,]
  d <- d[d$SPECIMEN_SEX_CODE == sex,]
  d <- d[!(is.na(d$VESSEL_ID)),]
  d <- d[!(is.na(d$Length_cm)),]

  if(plot.subfleet){
    d <- d[d$VESSEL_ID %in% subfleet.vrn,]
  }else{
    d <- d[!(d$VESSEL_ID %in% subfleet.vrn),]
  }

  yrs <- sort(unique(d$Year))
  len.dat <- lapply(1:length(yrs),
                    function(x){
                      d.yr <- d[d$Year == yrs[x],]
                      d.yr$Length_cm})

  ## Make all vectors the same length so they will cbind without replication
  max.len <- max(sapply(len.dat, length))
  len.dat <- lapply(len.dat,
                    function(x){
                      length(x) <- max.len
                      x})
  len.df <- do.call(cbind, len.dat)

  b <- boxplot(len.df, axes = FALSE, ylim = y.lim)
  axis(1,
       at = seq(1, length(yrs)),
       labels = yrs)
  axis(2,
       at = seq(y.lim[1], y.lim[2], by = 10),
       labels = seq(y.lim[1], y.lim[2], by = 10),
       las = 1)
  box()
  if(plot.subfleet){
    title.txt <- "Freezer trawlers - "
  }else{
    title.txt <- "Shoreside trawlers - "
  }
  if(sex == 1){
    title.txt <- paste0(title.txt, "Male")
  }else{
    title.txt <- paste0(title.txt, "Female")
  }
  title(title.txt)
  mtext("Year", 1, line = 3)
  mtext("Length (cm)", 2, line = 3)
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

plot.growth <- function(bio,
                        leg,
                        showtitle = TRUE,
                        add = FALSE){
  ## Plot the length/age data and fit from the bio global object
  ## If split sex, plot both with individual fits.
  ## First column of 'data' assumed to be length in mm, second is age.

  if(!add){
    oldPar <- par(no.readonly = TRUE)
    on.exit(par(oldPar))
  }

  legNames <- NULL
  legCols <- NULL
  data <- bio$vonb
  if(is.null(data)){
    cat0("Error - element 'vonb' of object 'bio' does not exist. Run the ",
         "VonB model from the Biotool tab.")
    return(NULL)
  }
  if(length(data) == 2){
    # Split sexes
    for(sex in 1:2){
      la <- data[[sex]][[1]]
      ## divide by ten to go from mm->cm
      l  <- la[,1]/10.0
      a  <- la[,2]
      if(sex == 1){
        col <- "blue"
        shade <- get.shade(col, 20)
        plot(a,
             l,
             col = shade,
             pch = 1,
             xlab = "Age",
             ylab = "Length (cm)")
      }else{
        col <- "red"
        shade <- get.shade(col, 20)
        points(a,
               l,
               col = shade,
               pch = 1,
               xlab = "Age",
               ylab = "Length (cm)")
      }
      linf <- data[[sex]][[2]][1,]
      k    <- data[[sex]][[2]][2,]
      tt0  <- data[[sex]][[2]][3,]
      curve(linf*(1-exp(-k*x)),
            col = col,
            lwd = 3,
            add = TRUE)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames,
                      as.expression(substitute(paste("Male  L"[infinity],
                                                     " = ",
                                                     linf,
                                                     " ",
                                                     kappa,
                                                     " = ",
                                                     k,
                                                     " tt"[0],
                                                     " = ",
                                                     tt0,
                                                     "\n"))))
      }else{
        legNames <- c(legNames,
                      as.expression(substitute(paste("Female  L"[infinity],
                                                     " = ",
                                                     linf,
                                                     " ",
                                                     kappa,
                                                     " = ",
                                                     k,
                                                     " tt"[0],
                                                     " = ",
                                                     tt0,
                                                     "\n"))))
      }
    }
  }else{
    # Combined sexes
    la  <- data[[1]][[1]]
    ## divide by ten to go from mm->cm
    l   <- la[,1]/10.0
    a   <- la[,2]
    col <- "blue"
    plot(a,
         l,
         col = col,
         xlab = "Age",
         ylab = "Length (cm)")
    linf <- data[[1]][[2]][1,]
    k    <- data[[1]][[2]][2,]
    tt0  <- data[[1]][[2]][3,]
    curve(linf*(1-exp(-k*x)),
          col = col,
          lwd = 3,
          add = TRUE)
    legCols <- c(legCols, col)
    legNames <- c(legNames,
                  as.expression(substitute(paste("Combined sexes  L"[infinity],
                                                 " = ",
                                                 linf,
                                                 " ",
                                                 kappa,
                                                 " = ",
                                                 k,
                                                 " tt"[0],
                                                 " = ",
                                                 tt0,
                                                 "\n"))))
   }
  if(!is.null(leg)){
    legend(leg,
           legend = legNames,
           col = legCols,
           lty = 1,
           lwd = 2)
  }
}

plot.lw <- function(bio,
                    leg,
                    showtitle = TRUE,
                    add = FALSE){
  ## Plot the length/weight data and fit from the bio global object
  ## If split sex, plot both with individual fits.
  ## First column of 'data' assumed to be length in mm, second is round
  ##  weight in grams.

  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  legNames <- NULL
  legCols <- NULL
  data <- bio$lw
  if(is.null(data)){
    cat0("Error - element 'lw' of object 'bio' does not exist. Run the ",
         "length/weight model from the Biotool tab.")
    return(NULL)
  }
  ## For alpha and beta as greek letters in plots
  greek <- c("alpha", "beta")
  cnames <- paste(LETTERS[1:2], letters[1:2])
  leg.exp <- sapply(1:2,
                      function(i){
                        as.expression(substitute(A (B),
                                                 list(A = as.name(cnames[i]),
                                                      B = as.name(greek[i]))))
                      })

  if(length(data) == 2){
    ## Split sexes
    for(sex in 1:2){
      lw <- data[[sex]][[1]]
      ## divide by ten to go from mm->cm
      l <- lw[,1] / 10.0
      w <- lw[,2]
      if(sex == 1){
        col <- "blue"
        shade <- get.shade(col, 20)
        plot(l,
             w,
             col = shade,
             pch = 1,
             xlab = "Length (cm)",
             ylab = "Weight (g)")
      }else{
        col <- "red"
        shade <- get.shade(col, 20)
        points(l,
               w,
               col = shade,
               pch = 1,
               xlab = "Length (cm)",
               ylab = "Weight (g)")
      }
      a <- data[[sex]][[2]][1,]
      b <- data[[sex]][[2]][2,]
      curve(a * x ^ b,
            col = col,
            lwd = 3,
            add = TRUE)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames,
                      as.expression(substitute(paste("Male  ",
                                                     alpha,
                                                     "=",
                                                     a,
                                                     " ",
                                                     beta,
                                                     "=",
                                                     b,
                                                     "\n"))))
      }else{
        legNames <- c(legNames,
                      as.expression(substitute(paste("Female  ",
                                                     alpha,
                                                     "=",
                                                     a,
                                                     " ",
                                                     beta,
                                                     "=",
                                                     b,
                                                     "\n"))))
      }
    }
  }else{
    ## Combined sexes
    lw <- data[[1]][[1]]
    ## divide by ten to go from mm->cm
    l <- lw[,1] / 10.0
    w <- lw[,2]
    col <- "blue"
    plot(l,
         w,
         col = col,
         xlab = "Length (cm)",
         ylab = "Weight (g)")
    a <- data[[1]][[2]][1,]
    b <- data[[1]][[2]][2,]
    curve(a * x ^ b,
          col = col,
          lwd = 3,
          add = TRUE)
    legCols <- c(legCols, col)
    legNames <- c(legNames,
                  as.expression(substitute(paste("Combined sexes  ",
                                                 alpha,
                                                 "=",
                                                 a,
                                                 " ",
                                                 beta,
                                                 "=",
                                                 b,
                                                 "\n"))))
  }
  if(!is.null(leg)){
    legend(leg,
           legend = legNames,
           col = legCols,
           lty = 1,
           lwd = 2)
  }
}

plot.ma <- function(bio,
                    leg = NULL,
                    showtitle = TRUE,
                    add = FALSE){
  ## Plot the maturity/age data and fit from the bio global object
  ## If split sex, plot both with individual fits.
  ## First column of 'data' assumed to be length in mm, second is
  ##  maturity level.

  if(!add){
    oldPar <- par(no.readonly=TRUE)
    on.exit(par(oldPar))
  }

  legNames <- NULL
  legCols <- NULL
  data <- bio$ma
  if(is.null(data)){
    cat0("Error - element 'ma' of object 'bio' does not exist. Run the ",
         "maturity/age model from the Biotool tab.")
    return(NULL)
  }
  if(length(data) == 2){
    ## Split sexes
    for(sex in 1:2){
      ma <- data[[sex]][[1]]
      a <- ma[,1]
      m <- ma[,2]
      #xlim <- c(0,max(a))
      xlim <- c(0, 25)
      if(sex == 1){
        col <- "blue"
        shade <- get.shade(col, 80)
        plot(a,
             m,
             col = shade,
             pch = 1,
             xlim = xlim,
             xlab = "Age",
             ylab = "Proportion mature")
      }else{
        col <- "red"
        shade <- get.shade(col, 80)
        points(a,
               m,
               col = shade,
               pch = 1,
               xlab = "Age",
               ylab = "Proportion mature")
      }
      a50 <- data[[sex]][[2]][1,]
      sigma_a50 <- data[[sex]][[2]][2,]
      curve(1 / (1 + exp(-(x - a50) / sigma_a50)),
            col = col,
            lwd = 3,
            add = TRUE)
      legCols <- c(legCols, col)
      if(sex == 1){
        legNames <- c(legNames,
                      as.expression(substitute(paste("Male  a"["50%"],
                                                     " = ",
                                                     a50,
                                                     " std"["50%"],
                                                     "  = ",
                                                     sigma_a50,
                                                     "\n"))))
      }else{
        legNames <- c(legNames,
                      as.expression(substitute(paste("Female  a"["50%"],
                                                     " = ",
                                                     a50,
                                                     " std"["50%"],
                                                     "  = ",
                                                     sigma_a50, "\n"))))
      }
    }
  }else{
    ## Combined sexes
    ma <- data[[1]][[1]]
    a <- ma[,1]
    m <- ma[,2]
    xlim <- c(0, max(a))
    col <- "blue"
    shade <- get.shade(col, 80)
    plot(a,
         m,
         col = shade,
         xlim = xlim,
         xlab = "Age",
         ylab = "Proportion mature")
    a50 <- data[[1]][[2]][1,]
    sigma_a50 <- data[[1]][[2]][2,]
    curve(1 / (1 + exp(-(x - a50) / sigma_a50)),
          col = col,
          lwd = 3,
          add = TRUE)
    legCols <- c(legCols, col)
    legNames <- c(legNames,
                  as.expression(substitute(paste("Combined sexes a"["50%"],
                                                 " = ",
                                                 a50,
                                                 " std"["50%"],
                                                 "  = ",
                                                 sigma_a50,
                                                 "\n"))))
  }
  if(!is.null(leg)){
    legend(leg,
           legend = legNames,
           col = legCols,
           lty = 1,
           lwd = 2)
  }
}

