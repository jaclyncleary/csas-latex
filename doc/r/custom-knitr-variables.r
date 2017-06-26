## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r

################################################################################
## Parameter values
r.quants <- base.model$mcmccalcs$r.quants
bo.lo <- r.quants["bo", 2]
bo.med <- r.quants["bo", 3]
bo.hi <- r.quants["bo", 4]
bmsy.lo <- r.quants["bmsy", 2]
bmsy.med <- r.quants["bmsy", 3]
bmsy.hi <- r.quants["bmsy", 4]
fmsy.lo <- r.quants["fmsy", 2]
fmsy.med <- r.quants["fmsy", 3]
fmsy.hi <- r.quants["fmsy", 4]

################################################################################
## Number of mcmc samples, min and max median biomass
b <- base.model[[1]]
num.mcmc.samples <- nrow(b$mcmc$params)
median.bio.min  <- f(min(b$mcmccalcs$sbt.quants[2,]), 3)
median.bio.min.year <- names(which.min(min(b$mcmccalcs$sbt.quants[2,])))
median.bio.max  <- f(max(b$mcmccalcs$sbt.quants[2,]), 3)
median.bio.max.year <- names(which.max(b$mcmccalcs$sbt.quants[2,]))

################################################################################
## Priors settings from the control file
base.m.prior.mean <- f(exp(base.model[[1]]$ctl$params[3, 6]), 1)
base.m.prior.sd <- f(base.model[[1]]$ctl$params[3, 7], 2)
sens.7.m.prior.mean <- f(exp(sens.models.4[[1]]$ctl$params[3, 6]), 1)
sens.7.m.prior.sd <- f(sens.models.4[[1]]$ctl$params[3, 7], 2)
sens.8.m.prior.mean <- f(exp(sens.models.4[[2]]$ctl$params[3, 6]), 1)
sens.8.m.prior.sd <- f(sens.models.4[[2]]$ctl$params[3, 7], 2)
sens.9.m.prior.mean <- f(exp(sens.models.5[[1]]$ctl$params[3, 6]), 1)
sens.9.m.prior.sd <- f(sens.models.5[[1]]$ctl$params[3, 7], 2)

################################################################################
## Age comp data values
## Gear indices, note they are not the indices in the model,
##  but the indices of the list elements in the model$dat$age.comps list
trawl.gear <- 1
qcsss.gear <- 2
hsss.gear <- 3
wcviss.gear <- 4
trawl.yrs <- sort(unique(base.model[[1]]$dat$age.comps[[trawl.gear]][,1]))
trawl.yrs <- paste0(min(trawl.yrs), "--", max(trawl.yrs))
qcsss.yrs <- sort(unique(base.model[[1]]$dat$age.comps[[qcsss.gear]][,1]))
qcsss.yrs <- paste(qcsss.yrs, collapse=", ")
hsss.yrs <- sort(unique(base.model[[1]]$dat$age.comps[[hsss.gear]][,1]))
hsss.yrs <- paste(hsss.yrs, collapse=", ")
wcviss.yrs <- sort(unique(base.model[[1]]$dat$age.comps[[wcviss.gear]][,1]))
wcviss.yrs <- paste(wcviss.yrs, collapse=", ")

################################################################################
## Values for posteriors
base.quants <- as.data.frame(base.model[[1]]$mcmccalcs$p.quants)
base.m.quants <- f(base.quants$m, 3)
base.bo.quants <- f(1000 * base.model[[1]]$mcmccalcs$r.quants[1, 2:4])

sens.7.m.quants <- f(as.data.frame(sens.models.4[[1]]$mcmccalcs$p.quants)$m, 3)
sens.7.bo.quants <- f(1000 * sens.models.4[[1]]$mcmccalcs$r.quants[1, 2:4])

sens.8.m.quants <- f(as.data.frame(sens.models.4[[2]]$mcmccalcs$p.quants)$m, 3)
sens.9.m.quants <- f(as.data.frame(sens.models.5[[1]]$mcmccalcs$p.quants)$m, 3)

################################################################################
## Catch data values
catch.yrs <- sort(unique(base.model[[1]]$dat$catch[,1]))
catch.yrs <- paste0(min(catch.yrs), "--", max(catch.yrs))

################################################################################
## Calculation of sigma and tau from rho and vartheta
rho <- base.model[[1]]$ctl$params[6, 1]
vartheta <- base.model[[1]]$ctl$params[7, 1]
tau <- f(sqrt((1 - rho) / vartheta), 1)
sigma <- f(sqrt(rho / vartheta), 1)
