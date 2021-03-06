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

# count up species per year
# count up how many species were studied each year
count_species_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies)
counted_species_for_cases <- count_species_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasivespecies)) %>%
  add_column("group" = rep("species"))
counted_species_by_year <- rename(counted_species_for_cases, n = `n_distinct(invasivespecies)`)
counted_species_by_year

# join
joined_species_and_cases <- left_join(counted_cases_per_year,counted_species_by_year, by = "publicationyear")
joined_species_and_cases

species_divided_by_cases <- mutate(joined_species_and_cases, spec_by_cases = n.y / n.x)
species_divided_by_cases

# summarize effect sizes by year
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)
ordered_by_year <- arrange(effect_sizes_richness_imputed, publicationyear)
head(ordered_by_year, 10)

# Now, take average of each year effect size from 1999-2016
average_effect_by_year <- ordered_by_year %>%
  group_by(publicationyear) %>%
  summarize(mean = abs(mean(yi)))
average_effect_by_year

# Now, add this new effect size data frame to species number data frame
species_and_effect_size_by_year <- right_join(species_divided_by_cases, average_effect_by_year)

# Make model either regular lm or 
# Note however: it's not standard to make a linear model for a fraction; 
#more common is a generalized linear model, which is a linear model along 
#with an invertible, nonlinear 'link' function that controls the range of the 
#desired model (here [0,1]).
# generalized linear model (glm) with a logit link and the binomial family
lm_effect_and_species <- lm(mean ~ spec_by_cases, data=species_and_effect_size_by_year)  # build linear regression model on full data
summary(lm_effect_and_species)
species_and_effect_size_by_year

glm_effect_and_species <- glm(mean ~ spec_by_cases, data = species_and_effect_size_by_year, family=quasibinomial(link="logit"))
summary(glm_effect_and_species)

gg_impact_factor <- ggplot(species_and_effect_size_by_year, aes(x=spec_by_cases, y=mean)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor
gg_impact_factor <- gg_impact_factor + scale_x_continuous(name = "species/cases") +
  scale_y_continuous(name = "effect size")
gg_impact_factor <- gg_impact_factor + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor

