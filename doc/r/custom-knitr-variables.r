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
fish.name <- "Pacific Herring"
science.name <- "Clupea pallasii"
family.name <- ""
common.name <- ""
bc <- "British Columbia"

################################################################################
## Values for assessment
##bo <- f(1000 * b.r.quants["bo", -1])
##bmsy <- f(1000 * b.r.quants["bmsy", -1])
##fmsy <- f(b.r.quants["fmsy", -1], 2)

##s6.r.quants <- as.data.frame(sens.models.3[[1]]$mcmccalcs$r.quants)
##sens.6.bo <- f(1000 * s6.r.quants["bo", -1])

sbt <- b.mcc$sbt.quants
sbt.final <- f(1000 * sbt[, ncol(sbt)])
sbt.final.yr <- as.numeric(colnames(sbt)[ncol(sbt)])
sbt.first.yr <- as.numeric(colnames(sbt)[1])

f.mort <- b.mcc$f.mort.quants[[1]]
max.f.mort <- max(f.mort[2,])
max.f.mort.ind <- which(f.mort[2,] == max.f.mort)
max.f.mort.yr <- colnames(f.mort)[max.f.mort.ind]
max.f.mort <- f(f.mort[, max.f.mort.ind], 3)
last.f.mort.yr <- as.numeric(colnames(f.mort)[ncol(f.mort)])
last.f.mort <- f(f.mort[, ncol(f.mort)], 3)

##depl <- b.mcc$depl.quants
##last.depl.yr <- colnames(depl)[ncol(depl)]
##last.depl <- f(depl[, ncol(depl)], 3)

survey.1.q <- f(b.p.quants$q1, 2)
survey.1.q <- f(b.p.quants$q2, 2)

trawl.a50 <- f(b.p.quants$sel1, 2)
qcsss.a50 <- f(b.p.quants$sel2, 2)

##s13.sel <- as.data.frame(sens.models.8[[2]]$ctl$sel)
##s13.trawl.a50 <- s13.sel[rownames(s13.sel) == "agelen50log", 1]

h.post <- f(b.p.quants$h, 3)
##sens.6.h.post <- f(as.data.frame(sens.models.3[[1]]$mcmccalcs$p.quants)$h, 3)

## Projection values and probabilities
tacs <- b$proj$tac.vec
min.tac <- f(1000 * min(tacs))
max.tac <- f(1000 * max(tacs))

tac.probs <- b.mcc$proj.dat
tac.0.prob <- f(100 * tac.probs[1, 4])
tac.30.prob <- f(100 * tac.probs[which(tac.probs[, 1] == 30), 4], 1)
tac.50.prob <- f(100 * tac.probs[which(tac.probs[, 1] == 50), 4], 1)

##tac.4bo <- tac.probs[, 5]
##min.tac.4b0 <- f(100 * min(tac.4bo), 1)
##max.tac.4b0 <- f(100 * max(tac.4bo), 1)

##tac.depl <- tac.probs[, 7]
##min.tac.depl <- f(100 * min(tac.depl), 1)
##max.tac.depl <- f(100 * max(tac.depl), 1)

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
##s6.priors <- as.data.frame(sens.models.3[[1]]$ctl$params)
##s7.priors <- as.data.frame(sens.models.4[[1]]$ctl$params)
##s8.priors <- as.data.frame(sens.models.4[[2]]$ctl$params)
##s9.priors <- as.data.frame(sens.models.5[[1]]$ctl$params)

## M priors
base.m.prior <- priors[rownames(priors) == "log_m",]
base.m.prior.mean <- f(exp(base.m.prior$p1), 1)
base.m.prior.sd <- f(base.m.prior$p2, 2)

##s7.prior <- s7.priors[rownames(s7.priors) == "log_m",]
##sens.7.m.prior.mean <- f(exp(s7.prior$p1), 1)
##sens.7.m.prior.sd <- f(s7.prior$p2, 2)

##s8.prior <- s8.priors[rownames(s8.priors) == "log_m",]
##sens.8.m.prior.mean <- f(exp(s8.prior$p1), 1)
##sens.8.m.prior.sd <- f(s8.prior$p2, 2)

##s9.prior <- s9.priors[rownames(s9.priors) == "log_m",]
##sens.9.m.prior.mean <- f(exp(s9.prior$p1), 1)
##sens.9.m.prior.sd <- f(s9.prior$p2, 2)

## Steepness prior for base
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

## Low steepness scenario
## sens.6.h.prior <- s6.priors[rownames(s6.priors) == "h",]
## sens.6.h.prior.alpha <- sens.6.h.prior$p1
## sens.6.h.prior.beta <- sens.6.h.prior$p2
## sens.6.h.prior.mean <- sens.6.h.prior.alpha / (sens.6.h.prior.alpha + sens.6.h.prior.beta)
## sens.6.h.prior.cv <- f(sqrt((sens.6.h.prior.alpha * sens.6.h.prior.beta) /
##                    ((sens.6.h.prior.alpha + sens.6.h.prior.beta)^2 *
##                     (sens.6.h.prior.alpha + sens.6.h.prior.beta + 1))) /
##                 sens.6.h.prior.mean, 2)
## sens.6.h.prior.alpha <- f(sens.6.h.prior.alpha, 1)
## sens.6.h.prior.beta <- f(sens.6.h.prior.beta, 1)
## sens.6.h.prior.mean <- f(sens.6.h.prior.mean, 2)

## Survey Q
q <- as.data.frame(b$ctl$surv.q)[,1]
q.mean <- f(exp(q[2]), 1)
q.sd <- f(q[3], 1)

## sens.10.q <- as.data.frame(sens.models.6[[1]]$ctl$surv.q)[,1]
## sens.10.q.mean <- f(exp(sens.10.q[2]), 1)
## sens.10.q.sd <- f(sens.10.q[3], 1)

## sens.11.q <- as.data.frame(sens.models.7[[1]]$ctl$surv.q)[,1]
## sens.11.q.mean <- f(exp(sens.11.q[2]), 1)
## sens.11.q.sd <- f(sens.11.q[3], 1)

################################################################################
## Age comp data values
## Gear indices, note they are not the indices in the model,
##  but the indices of the list elements in the model$dat$age.comps list
fishery.gear.1 <- 1
fishery.gear.2 <- 2
fishery.gear.3 <- 3
survey.gear.1 <- 4
survey.gear.2 <- 4
fishery.gear.1.yrs <- sort(unique(b$dat$age.comps[[fishery.gear.1]][,1]))
fishery.gear.1.yrs <- paste0(min(fishery.gear.1.yrs), "--", max(fishery.gear.1.yrs))
fishery.gear.2.yrs <- sort(unique(b$dat$age.comps[[fishery.gear.2]][,1]))
fishery.gear.2.yrs <- paste0(min(fishery.gear.2.yrs), "--", max(fishery.gear.2.yrs))
fishery.gear.3.yrs <- sort(unique(b$dat$age.comps[[fishery.gear.1]][,1]))
fishery.gear.3.yrs <- paste0(min(fishery.gear.3.yrs), "--", max(fishery.gear.3.yrs))

################################################################################
## Values for posteriors
base.m.quants <- f(b.p.quants$m, 3)
##base.bo.quants <- f(1000 * b.r.quants[1, 2:4])

##sens.7.m.quants <- f(as.data.frame(sens.models.4[[1]]$mcmccalcs$p.quants)$m, 3)
##sens.7.bo.quants <- f(1000 * sens.models.4[[1]]$mcmccalcs$r.quants[1, 2:4])

##sens.8.m.quants <- f(as.data.frame(sens.models.4[[2]]$mcmccalcs$p.quants)$m, 3)
##sens.9.m.quants <- f(as.data.frame(sens.models.5[[1]]$mcmccalcs$p.quants)$m, 3)

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

## MPD calculation for sigma and tau for sensitivity 3
##tau.3 <- f(sens.models.1[[2]]$mpd$tau, 2)
##sigma.3 <- f(sens.models.1[[2]]$mpd$sig, 2)

## Min and max proportions female for app-propfemale
##min.prop.female <- f(min(prop.female[,-1], na.rm = TRUE), 3)
##max.prop.female <- f(max(prop.female[,-1], na.rm = TRUE), 3)
