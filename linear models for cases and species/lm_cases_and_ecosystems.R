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
count_ecosystems_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies,ecosystemforheatmap)
counted_ecosystems_for_cases <- count_ecosystems_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(ecosystemforheatmap)) %>%
  add_column("group" = rep("ecosystem"))
counted_ecosystems_by_year <- rename(counted_ecosystems_for_cases, n = `n_distinct(ecosystemforheatmap)`)
counted_ecosystems_by_year
counted_ecosystems_by_year <- mutate(counted_ecosystems_by_year, div_by_ten = n/10)
counted_ecosystems_by_year

# Join
joined_eco_and_cases <- left_join(counted_cases_per_year,counted_ecosystems_by_year, by = "publicationyear")
joined_eco_and_cases

eco_divided_by_cases <- mutate(joined_eco_and_cases, eco_div_by_cases = n.y / n.x)
eco_divided_by_cases

# Summarize effect sizes
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
eco_and_effect_size_by_year <- right_join(eco_divided_by_cases, average_effect_by_year)

# Run a couple of models
eco_and_effect_size_by_year
lm_effect_and_eco <- lm(mean ~ eco_div_by_cases, data=eco_and_effect_size_by_year)  # build linear regression model on full data
summary(lm_effect_and_eco)

glm_effect_and_eco <- glm(mean ~ eco_div_by_cases, data = eco_and_effect_size_by_year, family=quasibinomial(link="logit"))
summary(glm_effect_and_eco)

#
gg_impact_factor <- ggplot(eco_and_effect_size_by_year, aes(x=eco_div_by_cases, y=mean)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor
gg_impact_factor <- gg_impact_factor + scale_x_continuous(name = "ecosystems/cases") +
  scale_y_continuous(name = "effect size")
gg_impact_factor <- gg_impact_factor + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor

