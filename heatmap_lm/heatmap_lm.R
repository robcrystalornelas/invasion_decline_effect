
## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(arrange)
library(tidyverse)
library(viridis)

## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

cases_and_publication_year <- dplyr::select(raw_data_imputed, code, publicationyear)

counted_ecosystems <- cases_and_publication_year %>%
  count(publicationyear) %>%
  add_column("group" = rep("cases"))
counted_ecosystems  

# linear model for pub year 
lm_ecosystems_and_year <- lm(n ~ publicationyear, data=counted_ecosystems)  # build linear regression model on full data
summary(lm_ecosystems_and_year)

