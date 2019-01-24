source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

head(raw_data_imputed)
count_species_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies)

distinct_code_and_publication_year <- distinct(code_and_publication_year)

# How many unique species are studied each year?
count_species_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasivespecies))

