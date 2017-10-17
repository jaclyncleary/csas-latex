
## make.likelihood.table(list(list(hg.b, hg.am2.constm, hg.am2.tvm),
##                            list(pr.b, pr.am2.constm, pr.am2.tvm),
##                            list(cc.b, cc.am2.constm, cc.am2.tvm),
##                            list(sog.b, wcvi.am2.constm, sog.am2.tvm),
##                            list(wcvi.b, wcvi.am2.constm, wcvi.am2.tvm)))

make.likelihood.table <- function(models,
                                  which = 1,
                                  digits = 3,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 11,
                                  space.size = 12,
                                  placement = "H"){
  ## digits - number of decimal places for the values
  ## which - 1 will put hg, prs, and cc in the table,
  ##         2 will put sog and wcvi in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  tab <- list()
  for(i in 1:length(models)){
    ## For each of the stocks 1 through 5
    ## Assume the first model is the base
    like <- lapply(models[[i]],
                   function(x){
                     nlvec <- as.vector(x$mpd$nlvec)
                     nlvec <- nlvec[!is.na(nlvec)]
                     nlvec <- nlvec[nlvec != 0]
                     -sum(nlvec)
                   })

    diffs <- lapply(like,
                    function(x){
                      like[[1]] - x
                    })
    ## Get the total number of parameters estimated for each model
    num.params <- lapply(models[[i]],
                         function(x){
                           ctl <- x$ctl
                           dat <- x$dat
                           params <- as.data.frame(ctl$params)
                           num.params <- x$mpd$NumParams
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

                           ## Natural mortality parameters
                           num.m.params <- 0
                           misc.ctl <- as.data.frame(ctl$misc)
                           if(misc.ctl["mdevphase", 1] > 0){
                             ## Natural mortality was estimated.
                             num.m.params <- misc.ctl["mnumestnodes", 1]
                           }

                           ## Catchability  parameters
                           ## q is a data frame with 1 column for each survey and 3 rows:
                           ## 1 - prior type:
                           ##      0) Uniformative prior
                           ##      1) normal prior density for log(q)
                           ##      2) random walk in q
                           ## 2 - prior log(mean)
                           ## 3 - prior SD

                           q <- ctl$surv.q
                           num.inds <- ctl$num.indices

                           ## Fishing mortality and recruitment parameters
                           ##
                           par <- x$par
                           num.f.params <- length(par$log_ft_pars)
                           num.rec.params <- length(par$log_rec_devs)
                           num.init.rec.params <- length(par$init_log_rec_devs)
                           tot <- num.params +
                             surv.est +
                             fish.est +
                             num.inds +
                             num.f.params +
                             num.rec.params +
                             num.init.rec.params +
                             num.m.params
                         })
    aic <- lapply(1:length(like),
                  function(x){
                    2 * num.params[[x]] - 2 * like[[x]]
                  })
    tab[[i]] <- as.data.frame(do.call(cbind, list(unlist(like),
                                                  unlist(diffs),
                                                  unlist(num.params),
                                                  unlist(aic))))
    tab[[i]][, 1] <- f(tab[[i]][, 1], 2)
    tab[[i]][, 2] <- f(tab[[i]][, 2], 2)
    tab[[i]][, 4] <- f(tab[[i]][, 4], 2)
  }
  tab <- do.call(rbind,
                 tab)

  tab <- cbind(rep(c("Time-varying M", "Constant M"),
                   length(models)),
               tab)

  tab <- cbind(c("HG",
                 "HG",
                 "PRD",
                 "PRD",
                 "CC",
                 "CC",
                 "SOG",
                 "SOG",
                 "WCVI",
                 "WCVI"),
               tab)

  colnames(tab) <- c(latex.bold("Area"),
                     latex.bold("Model"),
                     latex.bold("Likelihood"),
                     latex.mlc(c("Difference in",
                                 "likelihood from",
                                 "base")),
                     latex.mlc(c("Number of",
                                 "estimated",
                                 "parameters")),
                     latex.bold("AIC"))

  addtorow <- list()
  addtorow$pos <- list(2, 4, 6, 8)
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
