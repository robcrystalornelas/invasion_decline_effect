source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

head(raw_data_imputed)
# Get subset of columns for CMA
impact_factor_model_facet <- select(raw_data_imputed, code, publicationyear,invasivespeciestaxa,impactfactor)
head(impact_factor_model_facet)

# Subset by taxa
# algae
algae <- filter(impact_factor_model_facet, invasivespeciestaxa == "algae and seaweed")
linear_model_if_algae <- lm(impactfactor ~ publicationyear, data=algae)  # build linear regression model on full data
summary(linear_model_if_algae)

# aquatic plants
aquatic_plants <- filter(impact_factor_model_facet, invasivespeciestaxa == "aquatic plant")
linear_model_if_aquatic_plants <- lm(impactfactor ~ publicationyear, data=aquatic_plants)  # build linear regression model on full data
summary(linear_model_if_aquatic_plants)

# crusteacean
crust <- filter(impact_factor_model_facet, invasivespeciestaxa == "crustacean")
linear_model_if_crust <- lm(impactfactor ~ publicationyear, data=crust)  # build linear regression model on full data
summary(linear_model_if_crust)

# fish
fish <- filter(impact_factor_model_facet, invasivespeciestaxa == "fish")
linear_model_if_fish <- lm(impactfactor ~ publicationyear, data=fish)  # build linear regression model on full data
summary(linear_model_if_fish)

# grasses
grasses <- filter(impact_factor_model_facet, invasivespeciestaxa == "grasses")
linear_model_if_grasses <- lm(impactfactor ~ publicationyear, data=grasses)  # build linear regression model on full data
summary(linear_model_if_grasses)

#herbaceous plant
plant <- filter(impact_factor_model_facet, invasivespeciestaxa == "herbaceous plant")
linear_model_if_plant <- lm(impactfactor ~ publicationyear, data=plant)  # build linear regression model on full data
summary(linear_model_if_plant)

#insect
insect <- filter(impact_factor_model_facet, invasivespeciestaxa == "insect")
linear_model_if_insect <- lm(impactfactor ~ publicationyear, data=insect)  # build linear regression model on full data
summary(linear_model_if_insect)
plot(insect$publicationyear,insect$impactfactor)

# mammal
mammal <- filter(impact_factor_model_facet, invasivespeciestaxa == "mammal")
linear_model_if_mammal <- lm(impactfactor ~ publicationyear, data=mammal)  # build linear regression model on full data
summary(linear_model_if_mammal)

# molluscs
molluscs <- filter(impact_factor_model_facet, invasivespeciestaxa == "molluscs")
linear_model_if_molluscs <- lm(impactfactor ~ publicationyear, data=molluscs)  # build linear regression model on full data
summary(linear_model_if_molluscs)

# tree
tree <- filter(impact_factor_model_facet, invasivespeciestaxa == "tree")
linear_model_if_tree <- lm(impactfactor ~ publicationyear, data=tree)  # build linear regression model on full data
summary(linear_model_if_tree)

### Make ggplot graphs for the two significant publication year and impact factors
# fish and mammals

# basic linear regression plot
gg_impact_factor_fish <- ggplot(fish, aes(x=publicationyear, y=impactfactor)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor_fish
gg_impact_factor_fish <- gg_impact_factor_fish + scale_x_continuous(name = "Publication Year") +
  scale_y_continuous(name = "Effect size")
gg_impact_factor_fish <- gg_impact_factor_fish + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor_fish

gg_impact_factor_mammal <- ggplot(mammal, aes(x=publicationyear, y=impactfactor)) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor_mammal
gg_impact_factor_mammal <- gg_impact_factor_mammal + scale_x_continuous(name = "Publication Year") +
  scale_y_continuous(name = "Effect size")
gg_impact_factor_mammal <- gg_impact_factor_mammal + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor_mammal
