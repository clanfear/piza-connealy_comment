library(tidyverse)
segdata <- haven::read_dta("./piza_connealy/SPD_SegCrimeLong_001.dta")
library(fixest)

# N = 23,806 segments
reg_data <- segdata %>%
  mutate(week = (DAILY-1) %/% 6,
         actual_week = (DAILY-1) %/% 7,
         chop_treatment = CHOP * (DAILY >= 373),
         chop_buffer = CHOP_1304 * (DAILY >= 373),
         chop_precinct = CHOP_PRECI * (DAILY >= 373),
         treat_week = case_when(
           DAILY > 372 & DAILY <= 378 ~ 1,
           DAILY > 378 & DAILY <= 384 ~ 2,
           DAILY > 384 & DAILY <= 390 ~ 3,
           DAILY > 390 & DAILY <= 396 ~ 4
         ),
         treat_week_both = case_when(
           DAILY > 342 & DAILY <= 348 ~ -4,
           DAILY > 348 & DAILY <= 354 ~ -3 ,
           DAILY > 354 & DAILY <= 360 ~ -2 ,
           DAILY > 360 & DAILY <= 366 ~  -1,
           DAILY > 376 & DAILY <= 372 ~ 0,
           DAILY > 372 & DAILY <= 378 ~ 1,
           DAILY > 378 & DAILY <= 384 ~ 2,
           DAILY > 384 & DAILY <= 390 ~ 3,
           DAILY > 390 & DAILY <= 396 ~ 4
         ),
         chop_trend = chop_treatment * treat_week,
         week_n4 = (DAILY > 342 & DAILY <= 348),
         week_n3 = (DAILY > 348 & DAILY <= 354),
         week_n2 = (DAILY > 354 & DAILY <= 360),
         week_n1 = (DAILY > 360 & DAILY <= 366),
         week_0  = (DAILY > 366 & DAILY <= 372), 
         week_1  = (DAILY > 372 & DAILY <= 378),
         week_2  = (DAILY > 378 & DAILY <= 384),
         week_3  = (DAILY > 384 & DAILY <= 390),
         week_4  = (DAILY > 390 & DAILY <= 396)) %>%
  mutate(
    chop_week_n4 = CHOP * week_n4,
    chop_week_n3 = CHOP * week_n3,
    chop_week_n2 = CHOP * week_n2,
    chop_week_n1 = CHOP * week_n1,
    chop_week_0  = CHOP * week_0,
    chop_week_1  = CHOP * week_1,
    chop_week_2  = CHOP * week_2,
    chop_week_3  = CHOP * week_3,
    chop_week_4  = CHOP * week_4,
    chop_buffer_week_n4 = CHOP_1304 * week_n4,
    chop_buffer_week_n3 = CHOP_1304 * week_n3,
    chop_buffer_week_n2 = CHOP_1304 * week_n2,
    chop_buffer_week_n1 = CHOP_1304 * week_n1,
    chop_buffer_week_0  = CHOP_1304 * week_0,
    chop_buffer_week_1  = CHOP_1304 * week_1,
    chop_buffer_week_2  = CHOP_1304 * week_2,
    chop_buffer_week_3  = CHOP_1304 * week_3,
    chop_buffer_week_4  = CHOP_1304 * week_4,
    chop_precinct_week_n4 = CHOP_PRECI * week_n4,
    chop_precinct_week_n3 = CHOP_PRECI * week_n3,
    chop_precinct_week_n2 = CHOP_PRECI * week_n2,
    chop_precinct_week_n1 = CHOP_PRECI * week_n1,
    chop_precinct_week_0  = CHOP_PRECI * week_0,
    chop_precinct_week_1  = CHOP_PRECI * week_1,
    chop_precinct_week_2  = CHOP_PRECI * week_2,
    chop_precinct_week_3  = CHOP_PRECI * week_3,
    chop_precinct_week_4  = CHOP_PRECI * week_4,
  )

fepois(TOT_ ~ chop_treatment | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_trend | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_buffer | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_precinct | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_treatment + chop_buffer + chop_precinct | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_week_0 + chop_week_1 + chop_week_2 + chop_week_3 + chop_week_4 | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_week_n4 + chop_week_n3 + chop_week_n2 + chop_week_n1 + chop_week_0 + chop_week_1 + chop_week_2 + chop_week_3 + chop_week_4 + chop_week_0 | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_buffer_week_n4 + chop_buffer_week_n3 + chop_buffer_week_n2 + chop_buffer_week_n1 + chop_buffer_week_0 + chop_buffer_week_1 + chop_buffer_week_2 + chop_buffer_week_3 + chop_buffer_week_4 + chop_buffer_week_0 | ORIG_FID + DAILY, data = reg_data)
fepois(TOT_ ~ chop_precinct_week_n4 + chop_precinct_week_n3 + chop_precinct_week_n2 + chop_precinct_week_n1 + chop_precinct_week_0 + chop_precinct_week_1 + chop_precinct_week_2 + chop_precinct_week_3 + chop_precinct_week_4 + chop_precinct_week_0 | ORIG_FID + DAILY, data = reg_data)

segdata %>% filter(CHOP == 1)


# Can't seem to figure out the timing exactly:
# ARticle says period of analysis is from 6/1/2019 to 7/1/2020. That is 397 not 396 days. The code says it goes to 7/8/2020
# Appears first treatment is indeed time 373 as that matches code (last pre-treatment as 372)

# Period 1 is 6/8/2020 to 6/13/2020; final end is 7/1/2020
reg_data %>% 
  group_by(CHOP, week) %>% 
  summarize(mean_tot = mean(TOT_)) %>%
  ggplot(aes(x = week, y = mean_tot, group = CHOP, color = CHOP)) + geom_line() + geom_smooth(method = "gam") + geom_vline(xintercept = 62, color = "red") + scale_y_log10()

reg_data %>% 
  group_by(CHOP, actual_week) %>% 
  summarize(mean_tot = mean(TOT_)) %>%
  ggplot(aes(x = actual_week, y = mean_tot, group = CHOP, color = CHOP)) + geom_line() + geom_smooth(method = "gam") + scale_y_log10()

reg_data %>%
  group_by(CHOP, DAILY) %>% summarize(TOT_ = sum(TOT_)) %>% group_by(CHOP) %>% mutate(TOT_ = TOT_ / max(TOT_)) %>% ungroup() %>%
  ggplot(aes(x = DAILY, y = TOT_, group = CHOP, color = CHOP)) + geom_line() + geom_smooth(method = "gam") + geom_vline(xintercept = 372, color = "red")
