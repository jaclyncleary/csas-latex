## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r
##
## The variables defined here depend on the tructure of the
##  model-setup.r source code.

b <- base.model[[1]]

################################################################################
## Names used in assessment
fish.name <- "Arrowtooth Flounder"
science.name <- "Atheresthes stomias"
family.name <- "Pleuronectidae"
common.name <- "Turbot"
bc <- "British Columbia"

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

mc.quants <- as.data.frame(b$mcmccalcs$p.quants)
qcsss.q <- f(mc.quants$q1, 2)
hsmas.q <- f(mc.quants$q2, 2)
hsss.q <- f(mc.quants$q3, 2)
wcviss.q <- f(mc.quants$q4, 2)

trawl.a50 <- f(mc.quants$sel1, 2)

################################################################################
## Number of mcmc samples, min and max median biomass
mcmc.num.samples <- nrow(b$mcmc$params)
mcmc.burnin <- f(mcmc.num.samples - nrow(b$mcmccalcs$p.dat))
mcmc.num.samples <- f(mcmc.num.samples)
mcmc.length <- "15 million"
mcmc.samp.freq <- f(7500)
mcmc.ci <- "95\\%"

median.bio.min  <- f(min(b$mcmccalcs$sbt.quants[2,]), 3)
median.bio.min.year <- names(which.min(min(b$mcmccalcs$sbt.quants[2,])))
median.bio.max  <- f(max(b$mcmccalcs$sbt.quants[2,]), 3)
median.bio.max.year <- names(which.max(b$mcmccalcs$sbt.quants[2,]))

################################################################################
## Priors settings from the control file
priors <- as.data.frame(b$ctl$params)
s7.priors <- as.data.frame(sens.models.4[[1]]$ctl$params)
s8.priors <- as.data.frame(sens.models.4[[2]]$ctl$params)
s9.priors <- as.data.frame(sens.models.5[[1]]$ctl$params)

## M priors
base.m.prior <- priors[rownames(priors) == "log_m",]
base.m.prior.mean <- f(exp(base.m.prior$p1), 1)
base.m.prior.sd <- f(base.m.prior$p2, 2)

s7.prior <- s7.priors[rownames(s7.priors) == "log_m",]
sens.7.m.prior.mean <- f(exp(s7.prior$p1), 1)
sens.7.m.prior.sd <- f(s7.prior$p2, 2)

s8.prior <- s8.priors[rownames(s8.priors) == "log_m",]
sens.8.m.prior.mean <- f(exp(s8.prior$p1), 1)
sens.8.m.prior.sd <- f(s8.prior$p2, 2)

s9.prior <- s9.priors[rownames(s9.priors) == "log_m",]
sens.9.m.prior.mean <- f(exp(s9.prior$p1), 1)
sens.9.m.prior.sd <- f(s9.prior$p2, 2)

## Steepness prior
h.prior <- priors[rownames(priors) == "h",]
h.prior.alpha <- h.prior$p1
h.prior.beta <- h.prior$p2
h.prior.mean <- h.prior.alpha / (h.prior.alpha + h.prior.beta)
h.prior.cv <- f(sqrt((h.prior.alpha * h.prior.beta) /
                   ((h.prior.alpha + h.prior.beta)^2 *
                    (h.prior.alpha + h.prior.beta + 1))) /
                h.prior.mean, 2)
h.prior.alpha <- f(h.prior.alpha, 1)
h.prior.beta <- f(h.prior.beta, 1)
h.prior.mean <- f(h.prior.mean, 2)

################################################################################
## Age comp data values
## Gear indices, note they are not the indices in the model,
##  but the indices of the list elements in the model$dat$age.comps list
trawl.gear <- 1
qcsss.gear <- 2
hsss.gear <- 3
wcviss.gear <- 4
trawl.yrs <- sort(unique(b$dat$age.comps[[trawl.gear]][,1]))
trawl.yrs <- paste0(min(trawl.yrs), "--", max(trawl.yrs))
qcsss.yrs <- sort(unique(b$dat$age.comps[[qcsss.gear]][,1]))
qcsss.yrs <- paste(qcsss.yrs, collapse=", ")
hsss.yrs <- sort(unique(b$dat$age.comps[[hsss.gear]][,1]))
hsss.yrs <- paste(hsss.yrs, collapse=", ")
wcviss.yrs <- sort(unique(b$dat$age.comps[[wcviss.gear]][,1]))
wcviss.yrs <- paste(wcviss.yrs, collapse=", ")

################################################################################
## Values for posteriors
base.quants <- as.data.frame(b$mcmccalcs$p.quants)
base.m.quants <- f(base.quants$m, 3)
base.bo.quants <- f(1000 * b$mcmccalcs$r.quants[1, 2:4])

sens.7.m.quants <- f(as.data.frame(sens.models.4[[1]]$mcmccalcs$p.quants)$m, 3)
sens.7.bo.quants <- f(1000 * sens.models.4[[1]]$mcmccalcs$r.quants[1, 2:4])

sens.8.m.quants <- f(as.data.frame(sens.models.4[[2]]$mcmccalcs$p.quants)$m, 3)
sens.9.m.quants <- f(as.data.frame(sens.models.5[[1]]$mcmccalcs$p.quants)$m, 3)

################################################################################
## Catch data values
catch.yrs <- sort(unique(b$dat$catch[,1]))
catch.yrs <- paste0(min(catch.yrs), "--", max(catch.yrs))

################################################################################
## Calculation of sigma and tau from rho and vartheta
rho <- b$ctl$params[6, 1]
vartheta <- b$ctl$params[7, 1]
tau <- f(sqrt((1 - rho) / vartheta), 1)
sigma <- f(sqrt(rho / vartheta), 1)
