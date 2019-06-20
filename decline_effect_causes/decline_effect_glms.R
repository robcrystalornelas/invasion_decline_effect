## LOAD PACKAGES ####
library(metafor)
library(metaviz)
library(ggplot2)
library(lme4)
library(lmerTest)

# Read in data
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Check the data we are using
dim(raw_data_imputed)

# Get effect sizes for all studies in the database
# The output here, or the ROM, is the log-transformed ratio of means
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)

## METAFOR METHOD
# Metafor recommends fitting model first with a standard linear regression model
lm_with_all_predictors <- lm(yi ~ publicationyear + island_or_continent + impactfactor + invasive_trophic_position + time_since_invasion, data = effect_sizes_richness_imputed)
summary(lm_with_all_predictors)

# Then metafor recommends running the rma
effect_sizes_richness$vi <- 0
rma_with_all_predictors <- rma(yi, vi, mods = ~ publicationyear + island_or_continent + impactfactor + invasive_trophic_position, data = effect_sizes_richness_imputed, test = "knha")

# Then, compare the two model outputs
summary(lm_with_all_predictors)
rma_with_all_predictors

### GLM APPROACH
### If we want to take the GLM approach
# First, make a simple Generalized Linear Model (GLM) looking at decline effect over time
# Since our data are already long-transformed, we'll do a glm using a gaussian distribution.
# Since one of these elements is log transformed (yi), this means that one unit increase in 
glm_publication_year_only <- glm(yi ~ publicationyear, data = effect_sizes_richness_imputed)
summary(glm_publication_year_only)

glm_impact_factor_only <- glm(yi ~ impactfactor, data = effect_sizes_richness_imputed)
summary(glm_impact_factor_only)

# GLM with all predictors and no interaction
# GLM with island, and trophic, and publication year
glm_pub_island_trophic_if <- glm(yi ~ publicationyear + island_or_continent + invasive_trophic_position + impactfactor, data = effect_sizes_richness_imputed)

glm_island <- glm(yi ~ island_or_continent, data = effect_sizes_richness_imputed)
glm_island
summary(glm_island)
summary(glm_pub_island_trophic_if)

head(effect_sizes_richness_imputed)
# GLM with interaction ith interaction
# Before you can plot interaction, you need to center the predictor variables.
glm_with_pub_trophic_interaction <- glm(yi ~ publicationyear*invasive_trophic_position, data = effect_sizes_richness_imputed)
summary(glm_with_pub_trophic_interaction)
glm_with_pub_trophic_interaction

glm_with_pub_island_interaction <- glm(yi ~ publicationyear*island_or_continent, data = effect_sizes_richness_imputed)
summary(glm_with_pub_island_interaction)

glm_with_pub_if_interaction <- glm(yi ~ publicationyear*impactfactor, data = effect_sizes_richness_imputed)
summary(glm_with_pub_if_interaction)

rma_for_interaction <- rma(yi, vi, mods = ~ publicationyear*invasive_trophic_position, data = effect_sizes_richness_imputed, method = "DL")
rma_for_interaction

## Comparing GLM fits
AIC(glm_publication_year_only)
AIC(glm_with_pub_trophic_interaction)
AIC(glm_with_pub_island_interaction)
AIC(glm_with_pub_if_interaction)




