## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Analyze data ####
# Calculate effect size
head(raw_data_imputed)
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)
effect_sizes_richness

# Mixed effects by richness level
head(effect_sizes_richness)
mixed_effects_richness_level <- rma(yi, # outcome
                                   vi, # measure of variance
                                   mods = ~ richness_level_general - 1, # multiple moderating variables modeled as main effects
                                   method = "REML",
                                   data = effect_sizes_richness,
                                   slab = paste(lastname, publicationyear, sep = ""))

# Forest plot for richness level
forest_plot_richness_level <- viz_forest(x = mixed_effects_richness_level, 
                                        method = "REML",
                                        type = "summary_only",
                                        summary_label = c("higher","species"), 
                                        xlab = "Ratio of Means",
                                        col = "black",
                                        variant = "thick",
                                        text_size = 6,
#                                       summary_table = sample_size_richness_origin,
                                        annotate_CI = TRUE)
forest_plot_richness_level

