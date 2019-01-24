source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

head(raw_data_imputed)

# Calculate effect size
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)

effect_size_and_impact_factor <- select(effect_sizes_richness, yi, impactfactor)
effect_size_and_impact_factor_no_na <- na.omit(effect_size_and_impact_factor)
min(effect_size_and_impact_factor_no_na)

# make model
linear_model_impact_factor_and_effect_size <- lm(impactfactor ~ abs(yi), data=effect_size_and_impact_factor_no_na)  # build linear regression model on full data

# first, have to check if it's statistically significant
summary(linear_model_impact_factor_and_effect_size)

# basic linear regression plot
dim(effect_size_and_impact_factor_no_na)
head(effect_size_and_impact_factor_no_na)
length(effect_size_and_impact_factor_no_na$yi)

gg_impact_factor_and_effect <- ggplot(effect_size_and_impact_factor_no_na, aes(x=abs(yi), y=impactfactor)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor_and_effect
gg_impact_factor_and_effect <- gg_impact_factor_and_effect + scale_x_continuous(name = "effect size") +
  scale_y_continuous(name = "impact factor")
gg_impact_factor_and_effect <- gg_impact_factor_and_effect + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor_and_effect
