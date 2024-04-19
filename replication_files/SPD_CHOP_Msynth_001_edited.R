################################################################################
##INSTALLING PACKAGES
##INSTALL NECESSARY PACKAGES
##PACKAGES LISTED FOR DATA ASSESSMENT AND ANALYSIS


##UPDATE CSV READER TO BIG DATA
##IMPROVES EFFICIENCY WITH LARGE DATASETS

library(tidyverse)

##UPDATE MICROSYNTH ANALYSIS PACKAGE
##MOST RECENT SYNTHETIC CONTROL PACKAGE
library(microsynth)

cores <- 4
perms <- 100
################################################################################
##SET WORKING DIRECTORY AND RETRIEVE DATA
##ASSIGN CURRENT WORKING DIRECTORY WITH DATA AND R SCRIPT
##THESE COMPONENTS OF CODE WILL NEED UPDATED BASED ON SAVED LOCATION

##LOAD IN PREPPED DATA: CALLED "segdata" IN CODE
##DATASET IS PREPPED AND IN LONG FORM - MUST BE IN SAME FOLDER AS DIRECTORY
##IMPORT DTA AND/OR CSV - BETTER COMPATIBILITY WITH DIFFERENT PACKAGES
##segdata <- read_csv("SPD_SegCrimeLong_001.csv")

segdata <- haven::read_dta("./piza_connealy/SPD_SegCrimeLong_001.dta")
summary(segdata)
segdata <- as.data.frame(segdata)

################################################################################
##DATA ASSESSMENT
##SET SEED TO ENSURE RESULTS ARE REPLICATED IF ANY RANDOMNESS COMMANDS ARE USED
set.seed(10)

##ENSURE ALL VARIABLES ARE NUMERIC
##ALL VARIABLES SHOULD BE NUMERIC... ENSURE NONE WERE IMPORTED OR READ AS TEXT

##ENSURE THERE ARE NO ZEROES
##ONLY NON-RELEVANT DATA POINTS ARE 0 OR ARE TRUE 0
##MICROSYNTH PERFORMS BETTER WITH NON-ZEROES


##CHECK OBSERVATIONS FOR MISSING DATA
sum(complete.cases(segdata))
which(is.na(segdata))
unique (unlist (lapply (segdata, function (x) which (is.na (x)))))

segdata[is.na(segdata)] <- 0

# So, here I just take that one crazy outlier, the day the police return,
# and set the count equal to three days earlier, which is a median value of 
# total crime for the zone (3) across all days in the CHOP period.

segdata_noreturn <- segdata %>%
  group_by(CHOP) %>%
  arrange(ORIG_FID, DAILY) %>%
  mutate(TOT_ = ifelse(CHOP==1 & DAILY == 396, lag(TOT_, 3), TOT_))

save(segdata_noreturn, file = "./piza_connealy/data/segdata_noreturn.RData")

segdata %>% 
  filter(CHOP==1) %>% 
  group_by(DAILY) %>% 
  dplyr::summarize(TOT_ = sum(TOT_)) %>% ggplot(aes(x = DAILY, y = TOT_)) + geom_line()

segdata_noreturn %>% 
  filter(CHOP==1) %>% 
  group_by(DAILY) %>% 
  dplyr::summarize(TOT_ = sum(TOT_)) %>% ggplot(aes(x = DAILY, y = TOT_)) + geom_line()

################################################################################
##SET UP MICROSYNTH MODEL PARAMETERS
##ESTABLISH PRE/POST TEMPORAL THRESHOLDS
##NUMBER OF PERMUTATIONS
n_perm <- 999

##PRE AND POST INDIVIDUAL DAY START AND END POINTS
##CORRESPONDS TO DAILY VALUES ACROSS STUDY PERIOD 6/1/2019 - 7/8/2020
first_pre <- 1
end_pre <- 372
end_post <- 396

##BOOST MEMORY LIMITS FOR BIG DATA IN MICROSYNTH MODEL
##IMPROVES EFFICIENCY AND EXCEEDS MEMORY AND STORAGE LIMITS
memory.limit(size = 50000)

##EXAMINE ALL "TARGET" CHOP-BASED INTERVENTION VARIABLES
##CHOP ZONE ONLY / CHOP ZONE TWO BLOCKS / CHOP ZONE PRECINCT AREA - ONLY THREE OPERATIONALIZATIONS WERE TESTED AS TARGETS
##EXAMINE CHOP ONLY - 36 TARGET SEGMENTS
##CHOP ONLY VARIABLE IS "CHOP"
describe(segdata$CHOP)

##EXAMINE CHOP 2 BLOCKS - 273 TARGET SEGMENTS
##CHOP 2 BLOCK VARIABLE IS "CHOP_1304"
describe(segdata$CHOP_1304)

##EXAMINE CHOP PRECINCT AREA - 2860 TARGET SEGMENTS
##CHOP PRECINCT AREA VARIABLE IS "CHOP_PRECI"
describe(segdata$CHOP_PRECI)

##SET COVARIATES FOR MATCHING
##USE ALL TIME-INVARIANT COVARIATES TO REDUCE MODEL CONSTRAINTS
##COVARIATES CAN MATCH AS EXACT OR APPROXIMATE - LOOSER MATCHING RESTRICTION
cov_vars <- c('RTYPE', # Principal or arterial roadway
              'HCUNIT', # High crime units: Dichotomous if street with pre-chop total crime in 80th percentile
              'HCFSBEAT', # High call for service beat: Dichotomous if beat is in 80th percentile of calls
              'BIZ_TOTAL', # Total count of commercial businesses on unit
              'CONSUMER_T', # Total consumer facing establishments
              'LQHC', # High-crime location quotient
              'SLENGTH', # Total street length in target zone
              'CDI_MEAN', # Binary above or below mean concentrated disadvantage; unclear what unit
              'DEM_MEAN') # Dichotomized % non-white, age 15-29, vacants, owned/rented at blockgroup level

# ORIG_FID appears to be ID
# TOT_ Must be total crime count?
# CHOP, CHOP_1304, and CHOP_PRECI are indicators for treatment areas


##SET OUTCOME VARIABLE FOR ANALYSIS
##TOT = OUTCOME ALL CRIME VARIABLE
##SPECIFY OUTCOME VARIABLE TO MATCH AS PRE-CHOP PERIOD 6-DAY TOTAL CRIME BLOCKS
##LENGTH OF PRE-PERIOD = 62 6-DAY BLOCKS
out_vars6 <- list('TOT_'=c(6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                          6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6))

################################################################################
##CHOP ZONE ANALYSIS
##IDVAR IS UNIQUE RECORD IDENTIFIDER
##TIMEVAR IS READ AS "DAILY" CRIME VALUES BLOCKED AS 6-dAY CRIME COUNT TOTALS FROM 6/1/2019 - 7/8/2020 (62 BLOCKS)
##INTVAR = CHOP ZONE ONLY 
##END.POST PASSES A VECTOR TO ANALYZE THE POST-PERIOD IN 4 6-DAY BLOCKS
chop_only <- microsynth(segdata,
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
                        perm = perms,
                        n.cores = cores)
summary(chop_only)

##ANALYZE CHOP ONLY RESULTS 
##CREATE CHOP ZONE ONLY PLOTS
##SPECIFY DIFFERENT, CUMULATIVE POST-PERIOD 6-DAY BLOCKS WITH END POST
plot_microsynth(chop_only, start.pre = 1, end.pre = 372, end.post = 378)
plot_microsynth(chop_only, start.pre = 1, end.pre = 372, end.post = 384)
plot_microsynth(chop_only, start.pre = 1, end.pre = 372, end.post = 390)
plot_microsynth(chop_only, start.pre = 1, end.pre = 372, end.post = 396)

##SEND CHOP ZONE ONLY RESULTS TO TABULAR TXT FILE
summary(chop_only)
out <- capture.output(summary(chop_only))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopOnly.txt", sep="\n", append=TRUE)
save(chop_only, file = "./piza_connealy/output/chop_only.RData")

# WHAT ABOUT PERIOD BEFORE?
out_vars6_366 <- list('TOT_'=rep(6, length.out = 61))
chop_only_366 <- microsynth(segdata,
                            idvar="ORIG_FID",
                            timevar="DAILY",
                            intvar="CHOP",
                            start.pre=1,
                            end.pre = 366,
                            end.post= c(372,378,384,390,396),
                            check.feas=TRUE,
                            use.backup=TRUE,
                            match.covar = cov_vars,
                            match.out= out_vars6_366,
                            omnibus.var=FALSE,
                            confidence = .95,
                            test='two-sided',
                            use.survey = FALSE,
                            jack = FALSE,
                            perm = perms,
                            n.cores = cores)


summary(chop_only_366)
plot_microsynth(chop_only_366, start.pre = 1, end.pre = 366, end.post = 372)
out <- capture.output(summary(chop_only_366))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopOnly_366.txt", sep="\n", append=TRUE)
save(chop_only_366, file = "./piza_connealy/output/chop_only_366.RData")


# WHAT ABOUT WITHOUT THE JULY 1st SPIKE?
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
                            perm = perms,
                            n.cores = cores)


summary(chop_only_noreturn)
out <- capture.output(summary(chop_only_noreturn))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopOnly_noreturn.txt", sep="\n", append=TRUE)
save(chop_only_noreturn, file = "./piza_connealy/output/chop_only_noreturn.RData")

################################################################################
##CHOP ZONE 2 BLOCKS ANALYSIS
##IDVAR IS UNIQUE RECORD IDENTIFIDER
##TIMEVAR IS READ AS "DAILY" CRIME VALUES BLOCKED AS 6-dAY CRIME COUNT TOTALS FROM 6/1/2019 - 7/8/2020 (62 BLOCKS)
##INTVAR = CHOP ZONE 2 BLOCKS 
##END.POST PASSES A VECTOR TO ANALYZE THE POST-PERIOD IN 4 6-DAY BLOCKS
chop_2blk <- microsynth(segdata,
                        idvar="ORIG_FID",
                        timevar="DAILY",
                        intvar="CHOP_1304",
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
                        perm = perms,
                        n.cores = cores)
summary(chop_2blk)

##ANALYZE CHOP ZONE 2 BLOCKS RESULTS 
##CREATE CHOP ZONE 2 BLOCK PLOTS
##SPECIFY DIFFERENT, CUMULATIVE POST-PERIOD 6-DAY BLOCKS WITH END POST
plot_microsynth(chop_2blk, start.pre = 1, end.pre = 372, end.post = 378)
plot_microsynth(chop_2blk, start.pre = 1, end.pre = 372, end.post = 384)
plot_microsynth(chop_2blk, start.pre = 1, end.pre = 372, end.post = 390)
plot_microsynth(chop_2blk, start.pre = 1, end.pre = 372, end.post = 396)

##SEND CHOP ZONE 2 BLOCK RESULTS TO TABULAR TXT FILE
summary(chop_2blk)
out <- capture.output(summary(chop_2blk))
cat("MicroSynth Results", out, file="./piza_connealy/SW_Chop2Blk.txt", sep="\n", append=TRUE)
save(chop_2blk, file = "./piza_connealy/output/chop_2blk.RData")

## DO IT FOR WEEK BEFORE
# WHAT ABOUT PERIOD BEFORE?
chop_2blk_366 <- microsynth(segdata,
                            idvar="ORIG_FID",
                            timevar="DAILY",
                            intvar="CHOP_1304",
                            start.pre=1,
                            end.pre = 366,
                            end.post= c(372,378,384,390,396),
                            check.feas=TRUE,
                            use.backup=TRUE,
                            match.covar = cov_vars,
                            match.out= out_vars6_366,
                            omnibus.var=FALSE,
                            confidence = .95,
                            test='two-sided',
                            use.survey = FALSE,
                            jack = FALSE,
                            perm = perms,
                            n.cores = cores)
summary(chop_2blk_366)
out <- capture.output(summary(chop_2blk_366))
cat("MicroSynth Results", out, file="./piza_connealy/SW_Chop2Blk_366.txt", sep="\n", append=TRUE)
save(chop_2blk_366, file = "./piza_connealy/output/chop_2blk_366.RData")
################################################################################
##CHOP ZONE PRECINCT ANALYSIS
##IDVAR IS UNIQUE RECORD IDENTIFIDER
##TIMEVAR IS READ AS "DAILY" CRIME VALUES BLOCKED AS 6-dAY CRIME COUNT TOTALS FROM 6/1/2019 - 7/8/2020 (62 BLOCKS)
##INTVAR = CHOP ZONE PRECINCT AREA
##END.POST PASSES A VECTOR TO ANALYZE THE POST-PERIOD IN 4 6-DAY BLOCKS
chop_prec <- microsynth(segdata,
                        idvar="ORIG_FID",
                        timevar="DAILY",
                        intvar="CHOP_PRECI",
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
                        perm = perms,
                        n.cores = cores)
summary(chop_prec)

##ANALYZE CHOP ZONE PRECINCT AREA RESULTS 
##CREATE CHOP ZONE PRECINCT AREA PLOTS 
##SPECIFY DIFFERENT, CUMULATIVE POST-PERIOD 6-DAY BLOCKS WITH END POST
plot_microsynth(chop_prec, start.pre = 1, end.pre = 372, end.post = 378)
plot_microsynth(chop_prec, start.pre = 1, end.pre = 372, end.post = 384)
plot_microsynth(chop_prec, start.pre = 1, end.pre = 372, end.post = 390)
plot_microsynth(chop_prec, start.pre = 1, end.pre = 372, end.post = 396)

##SEND CHOP ZONE PRECINCT AREA RESULTS TO TABULAR TXT FILE
summary(chop_prec)
out <- capture.output(summary(chop_prec))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopPrecArea.txt", sep="\n", append=TRUE)
save(chop_prec, file = "./piza_connealy/output/chop_prec.RData")

# WEEK BEFORE
chop_prec_366 <- microsynth(segdata,
                            idvar="ORIG_FID",
                            timevar="DAILY",
                            intvar="CHOP_PRECI",
                            start.pre=1,
                            end.pre = 366,
                            end.post= c(372,378,384,390,396),
                            check.feas=TRUE,
                            use.backup=TRUE,
                            match.covar = cov_vars,
                            match.out= out_vars6_366,
                            omnibus.var=FALSE,
                            confidence = .95,
                            test='two-sided',
                            use.survey = FALSE,
                            jack = FALSE,
                            perm = perms,
                            n.cores = cores)
summary(chop_prec_366)
out <- capture.output(summary(chop_prec_366))
cat("MicroSynth Results", out, file="./piza_connealy/SW_ChopPrecArea_366.txt", sep="\n", append=TRUE)
save(chop_prec_366, file = "./piza_connealy/output/chop_prec_366.RData")
################################################################################




