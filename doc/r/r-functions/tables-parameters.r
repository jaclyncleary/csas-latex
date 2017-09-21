make.parameters.table <- function(model,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){
  ## Returns an xtable in the proper format for parameter estimates and priors
  ##
  ## xcaption - caption to appear in the calling document
  ## digits - number of digits after decimal point
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  get.bounds <- function(ind){
    ## Return the bounds string for row ind of the parameters
    paste0("[",
           params$lb[ind],
           ", ",
           params$ub[ind],
           "]")
  }

  get.vals <- function(ind){
    ## Return a 3-element vector for the number estimated, bounds, and prior
    ##  dist mean and SD or "Uniform" for the row ind of the parameters
    ##
    ## vec is a vector of 4 values, the phase, prior type (0=uniform, 1=normal,
    ##  2=lognormal, 3=beta, 4=gamma), the first and second parameter
    ##  of the prior.
    vec <- as.numeric(params[ind, 4:7])
    if(vec[1] < 1){
      return(c(0,
               "Fixed",
               paste0("$", f(params[ind, 1], 3), "$")))
    }else if(vec[2] == 0){
      return(c(1,
               get.bounds(ind),
               "Uniform"))
    }else if(vec[2] == 1){
      return(c(1,
               get.bounds(ind),
               paste0("Normal($ln(",
                      vec[4],
                      "), ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 2){
      return(c(1,
               get.bounds(ind),
               paste0("Lognormal($",
                      vec[3],
                      ", ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 3){
      return(c(1,
               get.bounds(ind),
               paste0("Beta($\\alpha = ",
                      vec[3],
                      ", \\beta = ",
                      vec[4],
                      "$)")))
    }else if(vec[2] == 4){
      return(c(1,
               get.bounds(ind),
               paste0("Gamma($k = ",
                      vec[3],
                      "), \\theta = ",
                      vec[4],
                      "$)")))
    }
    invisible()
  }

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  ctl <- model$ctl
  params <- as.data.frame(ctl$params)

  tab <- data.frame(param = character(),
                    num.est = character(),
                    bounds = character(),
                    prior = character(),
                    stringsAsFactors = FALSE)

  param.text <- c("Log recruitment ($ln(R_0)$)",
                  "Steepness ($h$)",
                  "Log natural mortality ($ln(M)$)",
                  "Log mean recruitment ($\\ln(\\overline{R})$)",
                  "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
                  "Variance ratio, rho ($\\rho$)",
                  "Inverse total variance, kappa ($\\kappa$)")

  param.vals <- do.call(rbind, lapply(1:nrow(params), get.vals))

  ## Selectivity parameters
  ## sel data frame has one column for each gear and 10 rows:
  ## 1  - selectivity type:
  ##       1) logistic selectivity parameters
  ##       2) selectivity coefficients
  ##       3) a constant cubic spline with age-nodes
  ##       4) a time varying cubic spline with age-nodes
  ##       5) a time varying bicubic spline with age & year nodes
  ##       6) fixed logistic (set isel_type=6, and estimation phase to -1)
  ##       7) logistic function of body weight.
  ##       8) logistic with weight deviations (3 parameters)
  ##       11) logistic selectivity with 2 parameters based on mean length
  ##       12) length-based selectivity coefficients with spline interpolation
  ## 2  - Age/length at 50% selectivity (logistic)
  ## 3  - STD at 50% selectivity (logistic)
  ## 4  - No. of age nodes for each gear (0=ignore)
  ## 5  - No. of year nodes for 2d spline(0=ignore)
  ## 6  - Phase of estimation (-1 for fixed) If neg number, it reflects a
  ##       mirroring of another gear's selectivity.
  ## 7  - Penalty wt for 2nd differences w=1/(2*sig^2)
  ## 8  - Penalty wt for dome-shaped w=1/(2*sig^2)
  ## 9  - Penalty wt for time-varying selectivity
  ## 10 - n_sel_blocks (number of selex blocks)

  sel <- ctl$sel
  dat <- model$dat
  indices <- dat$indices
  indices.df <- as.data.frame(do.call(rbind, indices))
  surv.gear.nums <- unique(indices.df$gear)
  surv.sel <- as.data.frame(sel[,surv.gear.nums])
  fish.sel <- as.data.frame(sel[,-surv.gear.nums])
  ## Get number estimated by looking at the phase row in the sel data frame
  surv.est <- surv.sel[6,]
  surv.est <- sum(surv.est > 0)
  fish.est <- fish.sel[6,]
  fish.est <- sum(fish.est > 0)
  ## Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  param.vals <- rbind(param.vals,
                      c(fish.est,
                        "[0, 1]",
                        "None"),
                      c(fish.est,
                        "[0, Inf)",
                        "None"))

  param.text <- c(param.text,
                "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)",
                "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)")

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  ## q <- ctl$surv.q
  ## num.inds <- ctl$num.indices
  ## param.vals <- rbind(param.vals,
  ##                     c(num.inds,
  ##                       "None",
  ##                       "Normal($0.5, 1$)"))

  ## param.text <- c(param.text,
  ##                 "Survey catchability ($q_k$)")

  ## Fishing mortality and recruitment parameters
  ##
  par <- model$par
  num.f.params <- length(par$log_ft_pars)
  num.rec.params <- length(par$log_rec_devs)
  num.init.rec.params <- length(par$init_log_rec_devs)
  param.vals <- rbind(param.vals,
                      c(num.rec.params,
                        "None",
                        "Normal($0, \\tau$)"),
                      c(num.init.rec.params,
                        "None",
                        "Normal($0, \\tau$)"))

  param.text <- c(param.text,
                  "Log recruitment deviations ($\\omega_t$)",
                  "Initial log recruitment deviations ($\\omega_{init,t}$)")

  tab <- cbind(param.text, param.vals)
  colnames(tab) <- c(latex.bold("Parameter"),
                     latex.mlc(c("Number",
                                 "estimated")),
                     latex.mlc(c("Bounds",
                                 "[low, high")),
                     latex.mlc(c("Prior (mean, SD)",
                                 "(single value = fixed)")))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement)
}

make.catchability.parameters.table <- function(am1.lst,
                                               am2.lst,
                                               xcaption = "default",
                                               xlabel   = "default",
                                               font.size = 9,
                                               space.size = 10,
                                               placement = "H"){
  ## am1.lst and am2.lst aree lists of the models AM1 and AM2
  ## to write q values for.
  ## xcaption - caption to appear in the calling document
  ## digits - number of digits after decimal point
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  lst <- list()
  st <- c("HG", "PRD", "CC", "SOG", "WCVI")
  for(i in 1:5){
    ## AM1
    ctl.am1 <- am1.lst[[i]][[1]]$ctl
    q.am1 <- as.data.frame(t(ctl.am1$surv.q))
    q.am1$priormeanlog <- exp(q.am1$priormeanlog)
    p.vals.am1.s <- paste0("Normal($",
                           f(q.am1$priormeanlog[1], 3),
                           ", ",
                           f(q.am1$priorsd[1], 3),
                           "$)")
    p.vals.am1.d <- paste0("Normal($",
                           f(q.am1$priormeanlog[2], 3),
                           ", ",
                           f(q.am1$priorsd[2], 3),
                           "$)")
    ## AM2
    ctl.am2 <- am2.lst[[i]][[1]]$ctl
    q.am2 <- as.data.frame(t(ctl.am2$surv.q))
    q.am2$priormeanlog <- exp(q.am2$priormeanlog)
    p.vals.am2.s <- paste0("Normal($",
                           f(q.am2$priormeanlog[1], 3),
                           ", ",
                           f(q.am2$priorsd[1], 3),
                           "$)")
    p.vals.am2.d <- paste0("Normal($",
                           f(q.am2$priormeanlog[2], 3),
                           ", ",
                           f(q.am2$priorsd[2], 3),
                           "$)")


    lst[[i]] <- rbind(c(st[i],
                      "AM1",
                      "Surface",
                      "None",
                      p.vals.am1.s),
                    c(st[i],
                      "AM1",
                      "Dive",
                      "None",
                      p.vals.am1.d),
                    c(st[i],
                      "AM2",
                      "Surface",
                      "None",
                      p.vals.am2.s),
                    c(st[i],
                      "AM2",
                      "Dive",
                      "None",
                      p.vals.am2.d))
  }
  tab <- do.call(rbind, lst)
  colnames(tab) <- c(latex.bold("SAR"),
                     latex.bold("Model"),
                     latex.bold("Survey"),
                     latex.bold("Bounds"),
                     latex.bold("Prior (mean, SD)"))

  addtorow <- list()
  addtorow$pos <- list(4, 8, 12, 16)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}

make.parameters.est.table <- function(model,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){
  ## Returns an xtable in the proper format for parameter estimates and priors
  ##
  ## digits - number of decimal points on % columns
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document


  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    if(pname == "m1"){
      pname <- "m"
    }
    if(pname == "bo"){
      pname <- "sbo"
    }
    match.sel <- grep("sel[[:digit:]]+",
                      pname)
    match.sel.sd <- grep("selsd[[:digit:]]+",
                         pname)
    match.q <- grep("q[[:digit:]]+",
                    pname)
    ## Age value at 50%
    sel.pars <- mpd$sel_par[,3]
    ## Age SD at 50%
    sel.sd.pars <- mpd$sel_par[,4]
    q.pars <- mpd$q
    if(length(match.sel) > 0){
      ## The parameter starts with "sel"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.pars[sel.num]
    }else if(length(match.sel.sd) > 0){
      ## The parameter starts with "selsd"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.sd.pars[sel.num]
    }else if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else{
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    j <- mpd.param.vals <- c(mpd.param.vals, this.par)
  }
  names(mpd.param.vals) <- mcmc.names
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- "MPD"
  rownames(tab) <- row.n
  tab <- f(t(tab), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.
  new.col <- c("$R_0$",
               "$Steepness (h)$",
               "$M$",
               "$\\overline{R}$",
               "$\\overline{R}_{init}$",
               "$\\rho$",
               "$\\vartheta$",
               "$B_0$",
               "$q_4$",
               "$q_5$",
               "$\\hat{a}_1$",
               "$\\hat{\\gamma}_1$",
               "$\\hat{a}_2$",
               "$\\hat{\\gamma}_2$",
               "$\\hat{a}_3$",
               "$\\hat{\\gamma}_3$")
  col.names <- colnames(tab)
  col.names <- latex.bold(latex.perc(col.names))
  col.names <- c(latex.bold("Parameter"), col.names)
  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

make.ref.points.table <- function(model,
                                  digits = 3,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){
  ## Returns an xtable in the proper format for reference points
  ##
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  tab <- model$mcmccalcs$r.quants
  tab[,-1] <- f(tab[,-1], digits)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

make.value.table <- function(model,
                             type,
                             syr,
                             digits = 3,
                             xcaption = "default",
                             xlabel   = "default",
                             font.size = 9,
                             space.size = 10,
                             placement = "H",
                             tabular.environment = "tabular"){
  ## Returns an xtable in the proper format for values (biomasas, recr, etc)
  ##
  ## syr - start year for table data
  ## type - 1=biomass, 2=recruitment, 3=F, 4=U, 5=depletion
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  col.names <- colnames(tab)
  col.names[1] <- "Year"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

make.biomass.depletion.table <- function(model,
                                         syr,
                                         digits = 3,
                                         xcaption = "default",
                                         xlabel   = "default",
                                         font.size = 9,
                                         space.size = 10,
                                         placement = "H",
                                         tabular.environment = "tabular"){
  ## Returns an xtable with both spawning biomass and depletion in it.
  ##  Based on make.value.table, but wider with both and extra headers
  ##
  ## syr - start year for table data
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  out.dat <- model$mcmccalcs$sbt.quants
  out.dat <- rbind(out.dat, model$mcmccalcs$depl.quants)

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]

  col.names <- colnames(tab)
  col.names[1] <- "Year"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- paste0("\\toprule",
                             latex.amp(),
                             latex.mcol(4,
                                        "c",
                                        latex.bold("Spawning Biomass")),
                             latex.amp(),
                             latex.mcol(4,
                                        "c",
                                        latex.bold("Relative Spawning Biomass")),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

make.sens.parameter.table <- function(tab,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){
  ## Returns an xtable of the sensitivity parameter information as found in
  ##  the CSV file in the data directory
  ## tab - the contents of the CSV file as read in by read.csv
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  ## Replace any | with a comma
  tab <- sub("\\|", ",", as.matrix(tab))
  colnames(tab) <- c(latex.bold("Scenario"),
                     latex.bold("Description"),
                     latex.bold("Parameters"))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

make.sens.q.table <- function(models,
                              model.names,
                              digits = 3,
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,
                              placement = "H"){
  ## Returns an xtable for the values of q, including quantiles
  ##
  ## models - list of model objects to give values for
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  quants <- lapply(models, function(x){
    t(f(x$mcmccalcs$q.quants, digits))})
  tab <- do.call(cbind, quants)
  tab <- cbind(rownames(tab), tab)
  col.names <- colnames(tab)
  col.names[1] <- "$q_k$"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  com <- paste0("\\toprule",
                latex.bold("Index"),
                latex.amp())
  for(i in 1:length(quants)){
    com <- paste0(com,
                  latex.mcol(ncol(quants[[i]]),
                             "c",
                             latex.bold(model.names[i])),
                  ifelse(i != length(quants), latex.amp(), ""))
  }
  com <- paste(com, latex.nline)
  addtorow$command <- com

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}
