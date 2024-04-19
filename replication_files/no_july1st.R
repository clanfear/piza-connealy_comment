library(tidyverse)
library(microsynth)
load("./piza_connealy/data/segdata_noreturn.RData")

cov_vars <- c('RTYPE', # Principal or arterial roadway
              'HCUNIT', # High crime units: Dichotomous if street with pre-chop total crime in 80th percentile
              'HCFSBEAT', # High call for service beat: Dichotomous if beat is in 80th percentile of calls
              'BIZ_TOTAL', # Total count of commercial businesses on unit
              'CONSUMER_T', # Total consumer facing establishments
              'LQHC', # High-crime location quotient
              'SLENGTH', # Total street length in target zone
              'CDI_MEAN', # Binary above or below mean concentrated disadvantage; unclear what unit
              'DEM_MEAN') # Dichotomized % non-white, age 15-29, vacants, owned/rented at blockgroup level
out_vars6 <- list('TOT_'=c(6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                           6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6))

chop_only_noreturn <- microsynth(segdata_noreturn,
                                 idvar="ORIG_FID",
                                 timevar="DAILY",
                                 intvar="CHOP",
                                 start.pre=1,
                                 end.pre = 372,
                                 end.post= c(378,384,390,396),
                                 check.feas=TRUE,
                                 use.backup=TRUE,
                                 match.covar = cov_vars,
                                 match.out= out_vars6,
                                 omnibus.var=FALSE,
                                 confidence = .95,
                                 test='two-sided',
                                 use.survey = FALSE,
                                 jack = FALSE,
                                 perm = 100,
                                 n.cores = 4)


summary(chop_only_noreturn)
out <- capture.output(summary(chop_only_noreturn))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopOnly_noreturn.txt", sep="\n", append=TRUE)
save(chop_only_noreturn, file = "./piza_connealy/output/chop_only_noreturn.RData")