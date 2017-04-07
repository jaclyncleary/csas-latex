## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r

################################################################################

################################################################################
## Attainment, used in the management performance section

################################################################################
## Recent catches

################################################################################
## TACs

################################################################################
## Survey values

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
## First forecast year depletion and spawning biomass estimates

################################################################################
## Number of mcmc samples, min and max median biomass
num.mcmc.samples <- nrow(base.model$mcmc$params)
median.bio.min  <- f(min(base.model$mcmccalcs$sbt.quants[2,]), 3)
median.bio.min.year <- names(which.min(min(base.model$mcmccalcs$sbt.quants[2,])))
median.bio.max  <- f(max(base.model$mcmccalcs$sbt.quants[2,]), 3)
median.bio.max.year <- names(which.max(base.model$mcmccalcs$sbt.quants[2,]))

################################################################################
## Prob biomass declines next year to year after with zero catch:
##zero.catch.prob.bio.down.1 <- f(base.model$risks[[1]][1,2])
## Prob biomass declines year after next to year after that with zero catch:
##zero.catch.prob.bio.down.2 <- f(base.model$risks[[2]][1,2])

################################################################################
## Second forecast year depletion and spawning biomass estimates

################################################################################
## number.to.word function located in utilities.r
##catches.below.200000.since.1986 <-
##  number.to.word(length(filter(catches, TOTAL <= 200000, Year > 1986)$Year))

################################################################################
## Age composition data for data section
## survey.age.years <- base.model$dat$agecomp[base.model$dat$agecomp$FltSvy == 2,]$Yr
## catch.limit.quantiles <- f(as.numeric(quantile(base.model$mcmc[[paste0("ForeCatch_", end.yr)]],
##                                                probs=c(0.025, 0.5, 0.975))))

## ################################################################################
## ## Estimated numbers at age for fishery for Recruitment section in Exec Summary and main text
## ##  From make.age.comp.fit.plot() which in turn calls age.fits()
## fishery.estimated.age.comp <- base.model$agedbase[base.model$agedbase$Fleet==1,]  #I think that this has ageing error incorporated
## year.class.2010.in.2013 <- f(filter(fishery.estimated.age.comp, Yr==2013, Bin==3)$Exp * 100)
## year.class.2010.in.2014 <- f(filter(fishery.estimated.age.comp, Yr==2014, Bin==4)$Exp * 100)
## year.class.2010.in.2015 <- f(filter(fishery.estimated.age.comp, Yr==2015, Bin==5)$Exp * 100)

## tmp <- base.model$catage[base.model$catage$Fleet==1,-(1:10)]  #This does not have ageing error
## fishery.estimated.age.comp <- cbind(base.model$catage[base.model$catage$Fleet==1,(1:10)],t(apply(tmp,1,function(x){x/sum(x)})))
## year.class.2010.in.2013 <- f(filter(fishery.estimated.age.comp, Yr==2013)$"3" * 100)
## year.class.2010.in.2014 <- f(filter(fishery.estimated.age.comp, Yr==2014)$"4" * 100)
## year.class.2010.in.2015 <- f(filter(fishery.estimated.age.comp, Yr==2015)$"5" * 100)
## year.class.2010.in.2016 <- f(filter(fishery.estimated.age.comp, Yr==2016)$"6" * 100)

## catcher.processor.catch <- f(100 * filter(catches, Year == last.data.yr)$atSea_US_CP / (last.year.us.cp.quota.reallocated), 1)
## mothership.catch <- f(100 * filter(catches, Year == last.data.yr)$atSea_US_MS / (last.year.us.ms.quota.reallocated), 1)
## shore.based.catch <- f(100 * filter(catches, Year == last.data.yr)$US_shore / (last.year.us.shore.quota.reallocated), 1)

## ################################################################################
## ## Canadian age data variables
## ## Canadian Freezer trawlers
## last.year.can.ages.ft <- can.ages[[2]][rownames(can.ages[[2]]) == last.data.yr,]
## get.age.prop(last.year.can.ages.ft, 1)
## ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 1)
## max.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
## max.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
## ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 2)
## second.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
## second.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
## ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 3)
## third.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
## third.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
## ft.age.prop.holder <- get.age.prop(last.year.can.ages.ft, 4)
## fourth.freezer.trawler.age.prop.age <- ft.age.prop.holder[1]
## fourth.freezer.trawler.age.prop <- f(ft.age.prop.holder[2] * 100, 1)
## ## Canadian Shoreside
## last.year.can.ages.ss <- can.ages[[1]][rownames(can.ages[[1]]) == last.data.yr,]
## ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 1)
## max.shoreside.age.prop.age <- ss.age.prop.holder[1]
## max.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
## ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 2)
## second.shoreside.age.prop.age <- ss.age.prop.holder[1]
## second.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
## ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 3)
## third.shoreside.age.prop.age <- ss.age.prop.holder[1]
## third.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)
## ss.age.prop.holder <- get.age.prop(last.year.can.ages.ss, 4)
## fourth.shoreside.age.prop.age <- ss.age.prop.holder[1]
## fourth.shoreside.age.prop <- f(ss.age.prop.holder[2] * 100, 1)

## ################################################################################
## ## Years for which median recruitment is below the mean of the median
## ##  recruitments for years >2010 and <(end.yr-1) ; end.yr-1 won't be
## ##  well estimated
## recruitment.med.since.2010 <- base.model$mcmccalcs$rmed[ which(as.numeric(names(base.model$mcmccalcs$rmed)) > 2010 & as.numeric(names(base.model$mcmccalcs$rmed)) < (end.yr-1))]
## years.since.2010.recruitment.med.below.mean <- names(recruitment.med.since.2010[recruitment.med.since.2010  < mean(base.model$mcmccalcs$rmed)])

## ################################################################################
## ## Exploitation values
## exploitation.med.2010 <- f(base.model$mcmccalcs$fmed["2010"],2)
## exploitation.med.penult.yr <- f(base.model$mcmccalcs$fmed[as.character(end.yr-1)],2)

## ################################################################################
## ## Survey comparisons of biomass from year to year. Use the table, not the value of survey.end.year
## ## Next year, we should set survey.end.yr to be what is in the table. Not going to attempt it
## ##  with only hours left to submission.
## last.survey.year <- survey.history[nrow(survey.history),]$year
## last.survey.year.biomass <- f(survey.history[nrow(survey.history),]$biomass * 10, 2) ## millions of tonnes
## penult.survey.year <- survey.history[nrow(survey.history) - 1,]$year
## penult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 1,]$biomass * 10, 2)
## antepenult.survey.year <- survey.history[nrow(survey.history) - 2,]$year
## antepenult.survey.year.biomass <- f(survey.history[nrow(survey.history) - 2,]$biomass * 10, 2)
## ## How many times higher is the last survey than the one before it?
## last.factor.penult <- f(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 1,]$biomass, 1)
## ## How many times higher is the last survey than the one that was two before it?
## last.factor.antepenult <- f(survey.history[nrow(survey.history),]$biomass / survey.history[nrow(survey.history) - 2,]$biomass, 1)

## ################################################################################
## ## Get priors settings from the control file
## param.details <- make.parameters.estimated.summary.table(base.model,
##                                                          start.rec.dev.yr = recruit.dev.start.yr,
##                                                          end.rec.dev.yr = end.yr - 1,
##                                                          return.xtable = FALSE)
## m.prior <- split.prior.info(param.details[rownames(param.details) == "m.vals",][4],
##                             dec.points = 2,
##                             first.to.lower = TRUE)
## ## Now, in document, use m.prior[1] for name of prior, m.prior[1] for mean, and m.prior[3] for SD.

## ################################################################################
## cohort.catch.1999 <- sum(cohortCatch(1999, base.model$catage))
## cohort.catch.2010 <- sum(cohortCatch(2010, base.model$catage))

## ################################################################################
## ## Sigma_r, standard deviation of recruitment variability.
## sigma.r <- f(base.model$sigma_R_in, 2)

## # alternative sigma.r based on all years of recdevs
## sigma.r.alt.allyr <- f(base.model$sigma_R_info$alternative_sigma_R[3],2)
## sigma.r.alt.main <- f(base.model$sigma_R_info$alternative_sigma_R[1],2)

## # range of "main" recdevs
## main.recdev.start <- min(base.model$recruit$year[base.model$recruit$era=="Main"])
## main.recdev.end <- max(base.model$recruit$year[base.model$recruit$era=="Main"])

## # range of "main" bias adjustement period for recdevs
## main.recdevbias.start <- min(base.model$recruit$year[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])
## main.recdevbias.end <- max(base.model$recruit$year[base.model$recruit$biasadj==max(base.model$recruit$biasadj)])


## ################################################################################
## ## Load weight-at-age file now that models are loaded
## wt.at.age <- load.wt.at.age(base.model, weight.at.age.file.name)

## ################################################################################
## ## Retrospective setup for the document. This must be done after the base.model
## ##  object has been fully setup.
## retro.model.names <- c(base.model.name,
##                        sapply(plot.retro.yrs,
##                               function(x) paste0("-", x, if(x == 1) " year" else " years")))
## ## Need to assemble the list with the base as the first element
## retro.list <- list(base.model)
## for(i in plot.retro.yrs){
##   retro.list[[i + 1]] <- base.model$retros[[i]]
## }
