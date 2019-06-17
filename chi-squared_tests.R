## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(arrange)
library(tidyverse)
library(viridis)
library(metafor)

# count up cases per year
cases_and_publication_year <- dplyr::select(raw_data_imputed, code, publicationyear)
dim(cases_and_publication_year)
# unique_articles <- distinct(cases_and_publication_year)
# dim(unique_articles)

# count cases per year
counted_cases_per_year <- cases_and_publication_year %>%
  count(publicationyear) %>%
  add_column("group" = rep("cases"))
counted_cases_per_year  

# count up trophic positions per year
# count up how many trophic levels were studied each year
count_trophic_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies,invasive_trophic_position)
count_trophic_by_year_for_cases <- count_trophic_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasive_trophic_position)) %>%
  add_column("group" = rep("invasive_trophic_position"))
count_trophic_by_year_for_cases
chisq.test(count_trophic_by_year_for_cases$`n_distinct(invasive_trophic_position)`)
