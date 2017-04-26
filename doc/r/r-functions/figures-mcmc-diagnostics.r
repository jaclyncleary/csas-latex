make.priors.posts.plot <- function(model,
                                   priors.only = TRUE){
  ## Make a plot of the priors used in the given model overlaid on the
  ##  posterior
  ##
  ## priors.only - Only plot the priors and not posteriors
  ##
  ## The values in the control file (model$ctl$param) for each
  ##  prior are:
  ## 1. ival  = initial value
  ## 2. lb    = lower bound
  ## 3. ub    = upper bound
  ## 4. phz   = ADMB phase
  ## 5. prior = prior distribution funnction
  ##             0 = Uniform
  ##             1 = normal    (p1=mu,p2=sig)
  ##             2 = lognormal (p1=log(mu),p2=sig)
  ##             3 = beta      (p1=alpha,p2=beta)
  ##             4 = gamma     (p1=alpha,p2=beta)
  ## 6. p1 (defined by 5 above)
  ## 7. p2 (defined by 5 above)

  f.names <- c(dunif, dnorm, dlnorm, dbeta, dgamma)

  mc <- model$mcmccalcs$p.dat
  ## Remove selectivity paramaters from the posts
  mc <- mc[, -grep("^sel.*", names(mc))]
  post.names <- names(mc)
  ## Log the values, except for steepness
  mc[, -grep("^h$", names(mc))] <- log(mc[, -grep("^h$", names(mc))])

  prior.specs <- as.data.frame(model$ctl$param)
  ## Remove fixed parameters
  prior.specs <- prior.specs[prior.specs$phz > 0,]
  ## Remove upper and lower bound, and phase information, but keep initial
  ##  value
  prior.specs <- prior.specs[, -c(2:4)]
  prior.names <- rownames(prior.specs)

  ## Add the q parameters to the prior specs table
  q.params <- model$ctl$surv.q
  num.q.params <- ncol(q.params)
  q.specs <- lapply(1:num.q.params,
                    function(x){
                      c(q.params[2, x],
                        q.params[1, x],
                        q.params[2, x],
                        q.params[3, x])
                    })
  q.specs <- as.data.frame(do.call(rbind, q.specs))
  rownames(q.specs) <- paste0("log_q", 1:num.q.params)
  colnames(q.specs) <- colnames(prior.specs)
  prior.specs <- rbind(prior.specs, q.specs)

  ## Get MPD estimates for the parameters in the posteriors
  mpd <- model$mpd
  q.pattern <- "^q([1-9]+)$"
  mpd.lst <- lapply(1:length(post.names),
                    function(x){
                      mle <- NULL
                      p.name <- post.names[x]
                      if(p.name == "m1"){
                        mle <- mpd$m[1]
                      }else if(p.name == "m2"){
                        mle <- mpd$m[2]
                      }else if(p.name == "h"){
                        mle <- mpd$steepness
                      }else if(length(grep(q.pattern, p.name)) > 0){
                        num <- as.numeric(sub(q.pattern, "\\1", p.name))
                        mle <- mpd$q[num]
                      }else{
                        mle <- as.numeric(mpd[post.names[x]])
                      }
                      mle})
  mpd.param.vals <- do.call(c, mpd.lst)
  names(mpd.param.vals) <- post.names

  n.side <- get.rows.cols(length(post.names))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  for(i in 1:length(post.names)){
    specs <- prior.specs[i,]
    prior.fn <- f.names[[as.numeric(specs[2] + 1)]]
    xx <- list(p = mc[,i],
               p1 = as.numeric(specs[3]),
               p2 = as.numeric(specs[4]),
               fn = prior.fn,
               nm = post.names[i],
               mle = as.numeric(mpd.param.vals[i]))
    xx$nm <- get.latex.name(xx$nm)

    if(priors.only){
      func <- function(x){xx$fn(x, xx$p1, xx$p2)}
      if(specs[2] == 0){
        ## Uniform, plot from p1-1 to p2+1
        curve(func,
              from = xx$p1 - 1,
              to = xx$p2 + 1,
              xlab = "",
              ylab = "",
              col = "green",
              lwd = 2)
      }else if(specs[2] == 1){
        ## Normal, plot from -(p1-p2*4) to (p1+p2*4)
        curve(func,
              from = xx$p1 - 4 * xx$p2,
              to = xx$p2 + 4 * xx$p2,
              xlab = "",
              ylab = "",
              col = "green",
              lwd = 2)
      }else{
        curve(func,
              xlab = "",
              ylab = "",
              col = "green",
              lwd = 2)
      }
    title(xx$nm)
    }else{
      plot.marg(xx,
                breaks = "sturges",
                col = "wheat")
    }
  }
}

plot.marg <- function(xx,
                      breaks = "sturges",
                      ex.factor = 1.0,
                      ...){
  ## xx - a list(p = samples, p1 = prior param 1, p2 = prior param 2,
  ##  fn = prior distribution)
  post.no.plot <- hist(as.matrix(xx$p),
                       breaks = breaks,
                       plot = FALSE)
  xvals <- seq(min(post.no.plot$breaks) / ex.factor,
               max(post.no.plot$breaks) / ex.factor,
               length = 1000)
  pd <- xx$fn(xvals, xx$p1, xx$p2)
  z <- cbind(xvals, pd)

  xlim <- c(min(xvals), max(xvals))
  ss <- hist(as.matrix(xx$p),
             prob = TRUE,
             breaks = breaks,
             main = xx$nm,
             xlab = "",
             cex.axis = 1.2,
             xlim = xlim,
             ylab = "",
             ...)
  func <- function(x){xx$fn(x, xx$p1, xx$p2)}
  ## Plot prior
  curve(func,
        xlim[1],
        xlim[2],
        xlab = "",
        ylab = "",
        col = "green",
        lwd = 2,
        add = TRUE)
  ## Plot MPD
  abline(v = xx$mle,
         lwd = 2,
         lty = 2,
         col = 2)
}

make.traces.plot <- function(model,
                             axis.lab.freq = 200){
  ## Make trace plots for all paramaters from the mcmc output
  ## axis.lab.freq - the frequency of x-axis labelling

  mc <- model$mcmccalcs$p.dat
  mc <- model$mcmc$params
  n.side <- get.rows.cols(ncol(mc))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  for(param in 1:ncol(mc)){
    mcmc.trace <- as.matrix(mc[,param])
    name <- colnames(mc)[param]
    name <- get.latex.name(name)
    plot(mcmc.trace,
         main = name,
         type = "l",
         ylab = "",
         xlab = "",
         axes = FALSE)
    box()
    at <- labels <- seq(0,
                        nrow(mc),
                        axis.lab.freq)
    axis(1,
         at = at,
         labels = labels)
    axis(2)
  }
}
