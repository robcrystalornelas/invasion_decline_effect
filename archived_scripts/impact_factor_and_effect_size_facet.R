source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

head(raw_data_imputed)

effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)

# Get subset of columns for CMA
impact_factor_and_effect_facet <- select(effect_sizes_richness, code,invasivespeciestaxa, yi,impactfactor)
head(impact_factor_and_effect_facet)

# Subset by taxa
# algae
algae <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "algae and seaweed")
linear_model_if_effect_algae <- lm(impactfactor ~ abs(yi), data=algae)  # build linear regression model on full data
summary(linear_model_if_effect_algae)

# aquatic plants
aquatic_plants <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "aquatic plant")
linear_model_if_effect_aquatic_plants <- lm(impactfactor ~ abs(yi), data=aquatic_plants)  # build linear regression model on full data
summary(linear_model_if_effect_aquatic_plants)

# crusteacean
crust <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "crustacean")
linear_model_if_effect_crust <- lm(impactfactor ~ abs(yi), data=crust)  # build linear regression model on full data
summary(linear_model_if_effect_crust)

# fish
fish <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "fish")
linear_model_if_effect_fish <- lm(impactfactor ~ abs(yi), data=fish)  # build linear regression model on full data
summary(linear_model_if_effect_fish)

# grasses
grasses <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "grasses")
linear_model_if_effect_grasses <- lm(impactfactor ~ abs(yi), data=grasses)  # build linear regression model on full data
summary(linear_model_if_effect_grasses)

#herbaceous plant
plant <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "herbaceous plant")
linear_model_if_effect_plant <- lm(impactfactor ~ abs(yi), data=plant)  # build linear regression model on full data
summary(linear_model_if_effect_plant)

#insect
insect <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "insect")
linear_model_if_effect_insect <- lm(impactfactor ~ abs(yi), data=insect)  # build linear regression model on full data
summary(linear_model_if_effect_insect)


# mammal
mammal <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "mammal")
linear_model_if_effect_insect <- lm(impactfactor ~ abs(yi), data=mammal)  # build linear regression model on full data
summary(linear_model_if_effect_insect)

# molluscs
molluscs <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "molluscs")
linear_model_if_effect_molluscs <- lm(impactfactor ~ abs(yi), data=molluscs)  # build linear regression model on full data
summary(linear_model_if_effect_molluscs)
plot(abs(molluscs$yi),molluscs$impactfactor)

# tree
tree <- filter(impact_factor_and_effect_facet, invasivespeciestaxa == "tree")
linear_model_if_effect_tree <- lm(abs(yi) ~ impactfactor, data=tree)  # build linear regression model on full data
summary(linear_model_if_effect_tree)

