## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r
##
## The variables defined here depend on the tructure of the
##  model-setup.r source code.

## Base model and some of its outputs simplified
b <- base.model[[1]]
b.params <- as.data.frame(b$mcmc$params)
b.mcc <- b$mcmccalcs
b.p.quants <- as.data.frame(b.mcc$p.quants)
b.r.quants <- as.data.frame(b.mcc$r.quants)

################################################################################
## Names used in assessment
fish.name <- "Arrowtooth Flounder"
science.name <- "Atheresthes stomias"
family.name <- "Pleuronectidae"
common.name <- "Turbot"
bc <- "British Columbia"

################################################################################
## Values for assessment
bo <- f(1000 * b.r.quants["bo", -1])
bmsy <- f(1000 * b.r.quants["bmsy", -1])
fmsy <- f(b.r.quants["fmsy", -1], 2)

sbt <- b.mcc$sbt.quants
sbt.final <- f(1000 * sbt[, ncol(sbt)])

qcsss.q <- f(b.p.quants$q1, 2)
hsmas.q <- f(b.p.quants$q2, 2)
hsss.q <- f(b.p.quants$q3, 2)
wcviss.q <- f(b.p.quants$q4, 2)

trawl.a50 <- f(b.p.quants$sel1, 2)
qcsss.a50 <- f(b.p.quants$sel2, 2)
hsss.a50 <- f(b.p.quants$sel4, 2)
wcviss.a50 <- f(b.p.quants$sel5, 2)

################################################################################
## Number of mcmc samples, min and max median biomass
mcmc.num.samples <- nrow(b.params)
mcmc.burnin <- f(mcmc.num.samples - nrow(b.mcc$p.dat))
mcmc.num.samples <- f(mcmc.num.samples)
mcmc.length <- "15 million"
mcmc.samp.freq <- f(7500)
mcmc.ci <- "95\\%"

median.bio.min  <- f(min(b.mcc$sbt.quants[2,]), 3)
median.bio.min.year <- names(which.min(min(b.mcc$sbt.quants[2,])))
median.bio.max  <- f(max(b.mcc$sbt.quants[2,]), 3)
median.bio.max.year <- names(which.max(b.mcc$sbt.quants[2,]))

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
base.m.quants <- f(b.p.quants$m, 3)
base.bo.quants <- f(1000 * b.r.quants[1, 2:4])

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
