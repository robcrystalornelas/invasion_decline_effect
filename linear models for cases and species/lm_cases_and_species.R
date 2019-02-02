## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(arrange)
library(tidyverse)
library(viridis)

# count up cases per year
counted_cases <- cases_and_publication_year %>%
  count(publicationyear) %>%
  add_column("group" = rep("cases"))
counted_cases  

# count up species per year
# count up how many species were studied each year
count_species_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies)
counted_species_for_cases <- count_species_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasivespecies)) %>%
  add_column("group" = rep("species"))
counted_species_for_cases <- rename(counted_species_for_cases, n = `n_distinct(invasivespecies)`)
counted_species_for_cases

# Run each model separately
lm_cases_and_year <- lm(n ~ publicationyear, data=counted_cases)  # build linear regression model on full data
summary(lm_cases_and_year)

lm_species_and_year <- lm(n ~ publicationyear, data = joined_species_and_cases)
summary(lm_species_and_year)

head(joined_species_and_cases)
lm_species_and_cases <- lm(n ~ publicationyear + group, data = joined_species_and_cases)

anova(lm_species_and_year, lm_species_and_cases)

# compare them both using a GLM
# Join up two tibbles
joined_species_and_cases <- full_join(counted_species_for_cases,counted_cases)
joined_species_and_cases

# establish glm formula
glm_form <- n ~ publicationyear * group
model.results <- glm(glm_form, data=joined_species_and_cases, family=poisson)
model.results

joined_species_and_cases
glm_form <- n ~ group + publicationyear
model.results <- glm(glm_form, data=joined_species_and_cases, family=poisson)
model.results
summary(model.results)

