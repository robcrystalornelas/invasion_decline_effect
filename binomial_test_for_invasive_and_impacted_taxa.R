
## LOAD PACKAGES ####
library(metafor)
library(metaviz)
library(ggplot2)
library(lme4)
library(lmerTest) # lme4 doesn't give p-values, but this package adds them
library(cowplot)


# Read in data
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

match_or_no_match <- plyr::count(raw_data_imputed$impact_and_invasive_match)
binom.test(142, 334,p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)

