## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Clean data ####
dim(raw_data_imputed)
unique(raw_data_imputed$code)
## Analyze data ####
# Effect size is Log transformed ratio of means
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)

# Meta-regression for study length
head(effect_sizes_richness_imputed)
mixed_effects_richness_level <- rma(yi, # outcome
                                  vi, # measure of variance
                                  mods = ~ richness_level_general - 1, # multiple moderating variables modeled as main effects
                                  method = "REML",
                                  data = effect_sizes_richness_imputed,
                                  slab = paste(lastname, publicationyear, sep = ""))
mixed_effects_richness_level

# Make forest plot
forest_plot_richness_level <- viz_forest(x = mixed_effects_richness_level, 
                                       method = "REML",
                                       type = "summary_only",
                                       xlab = "response ratio",
                                       col = "black",
                                       variant = "thick",
                                      # summary_table = sample_size_table_study_length,
                                      # table_headers = c("study length","sample size"),
                                       text_size = 7,
                                       annotate_CI = TRUE)
forest_plot_richness_level
