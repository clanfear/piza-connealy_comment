################################################################################
##INSTALLING PACKAGES
##INSTALL NECESSARY PACKAGES
##PACKAGES LISTED FOR DATA ASSESSMENT AND ANALYSIS
install.packages(c("Rcpp", "microsynth"))
install.packages("dplyr")
library(haven)
library(ggplot2)
library(Hmisc)
install.packages("foreign")
library(foreign)
library(haven)

##UPDATE CSV READER TO BIG DATA
##IMPROVES EFFICIENCY WITH LARGE DATASETS
install.packages("readr")
install.packages("tidyverse")
library(tidyverse)

##UPDATE MICROSYNTH ANALYSIS PACKAGE
##MOST RECENT SYNTHETIC CONTROL PACKAGE
library(microsynth)

################################################################################
##SET WORKING DIRECTORY AND RETRIEVE DATA
##ASSIGN CURRENT WORKING DIRECTORY WITH DATA AND R SCRIPT
##THESE COMPONENTS OF CODE WILL NEED UPDATED BASED ON SAVED LOCATION
getwd()
setwd("C:\\Users\\NConn\\OneDrive\\Documents\\PROJECTS\\Seattle\\SW_RCODE")

##LOAD IN PREPPED DATA: CALLED "segdata" IN CODE
##DATASET IS PREPPED AND IN LONG FORM - MUST BE IN SAME FOLDER AS DIRECTORY
##IMPORT DTA AND/OR CSV - BETTER COMPATIBILITY WITH DIFFERENT PACKAGES
##segdata <- read_csv("SPD_SegCrimeLong_001.csv")
segdata <- read_dta("SPD_SegCrimeLong_001.dta")
summary(segdata)
segdata <- as.data.frame(segdata)

################################################################################
##DATA ASSESSMENT
##SET SEED TO ENSURE RESULTS ARE REPLICATED IF ANY RANDOMNESS COMMANDS ARE USED
set.seed(10)

##ENSURE ALL VARIABLES ARE NUMERIC
##ALL VARIABLES SHOULD BE NUMERIC... ENSURE NONE WERE IMPORTED OR READ AS TEXT
sapply(data, mode)
sapply(data, class)

##ENSURE THERE ARE NO ZEROES
##ONLY NON-RELEVANT DATA POINTS ARE 0 OR ARE TRUE 0
##MICROSYNTH PERFORMS BETTER WITH NON-ZEROES
segdata[is.na(segdata)] <- 0

##CHECK OBSERVATIONS FOR MISSING DATA
sum(complete.cases(segdata))
which(is.na(segdata))
unique (unlist (lapply (segdata, function (x) which (is.na (x)))))

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
cov_vars <- c('RTYPE','HCUNIT','HCFSBEAT', 'BIZ_TOTAL', 'CONSUMER_T', 'LQHC', 'SLENGTH', 'CDI_MEAN', 'DEM_MEAN')

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
                        perm = 999,
                        n.cores = FALSE)
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
cat("MicroSynth Results", out, file="SW_ChopOnly.txt", sep="\n", append=TRUE)

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
                        perm = 999,
                        n.cores = FALSE)
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
cat("MicroSynth Results", out, file="SW_Chop2Blk.txt", sep="\n", append=TRUE)

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
                        perm = 999,
                        n.cores = FALSE)
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
cat("MicroSynth Results", out, file="SW_ChopPrecArea.txt", sep="\n", append=TRUE)
################################################################################




