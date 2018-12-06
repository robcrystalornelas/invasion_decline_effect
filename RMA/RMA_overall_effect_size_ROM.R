## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Clean data ####
dim(raw_data)
tail(raw_data)

## Analyze data ####
# Effect size is Log transformed ratio of means
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)


# Random effects model
head(effect_sizes_richness)
dim(effect_sizes_richness)
random_effects_model <- rma(yi=effect_sizes_richness$yi, 
                            vi=effect_sizes_richness$vi,
                            method = "REML",
                            test = "knha",
                            weights=effect_sizes_richness$total_sample_size,
                            data=effect_sizes_richness)
random_effects_model

# Random effects model forest plot
forest_plot_random_effects <- viz_forest(
  x = random_effects_model, 
  method = "REML",
  study_labels = effect_sizes_richness[1:328, "code"], # include study name label
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues"
  #  variant = "thick"
)
forest_plot_random_effects
