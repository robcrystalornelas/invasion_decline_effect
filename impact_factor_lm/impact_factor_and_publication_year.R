## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

# Get subset of columns for CMA
impact_factor_model <- select(raw_data_imputed, code, publicationyear,impactfactor)
head(impact_factor_model)

# make model
linear_model_impact_factor <- lm(impactfactor ~ publicationyear, data=impact_factor_model)  # build linear regression model on full data

# first, have to check if it's statistically significant
summary(linear_model_impact_factor)

# basic linear regression plot
gg_impact_factor <- ggplot(impact_factor_model, aes(x=publicationyear, y=impactfactor)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
gg_impact_factor
gg_impact_factor <- gg_impact_factor + scale_x_continuous(name = "Publication Year") +
  scale_y_continuous(name = "Effect size")
gg_impact_factor <- gg_impact_factor + theme_bw() + theme(text = element_text(size = 20))
gg_impact_factor

gg_impact_factor
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/linear_model_impact_factor.pdf")
gg_impact_factor
dev.off()
dev.off()
