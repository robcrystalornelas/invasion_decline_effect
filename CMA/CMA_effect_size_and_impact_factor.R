## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

# Calculate effect size
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)

## Run a cumulative MA
## first, order studies by year
ordered_by_impact_factor <- arrange(effect_sizes_richness, impactfactor)
head(ordered_by_impact_factor)

# First, get the results of the random effects model
random_effects_model_impact_factor <- rma(yi=ordered_by_impact_factor$yi, 
                                    vi=ordered_by_impact_factor$vi,
                                    method = "REML",
                                    test = "knha",
                                    data=ordered_by_impact_factor)
random_effects_model_impact_factor

forest_plot_impact_factor_CMA <- viz_forest(
  x = random_effects_model_impact_factor, 
  method = "REML",
  study_labels = ordered_by_impact_factor[1:334, "impactfactor"], # include study name label
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative")

forest_plot_impact_factor_CMA
