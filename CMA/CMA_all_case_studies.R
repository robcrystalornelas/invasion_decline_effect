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
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)

## Run a cumulative MA
## first, order studies by year
ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year)

# Random effects model
random_effects_model_ordered <- rma(yi=ordered_by_year$yi, 
                            vi=ordered_by_year$vi,
                            method = "REML",
                            test = "knha",
                            weights=ordered_by_year$total_sample_size,
                            data=ordered_by_year)
random_effects_model_ordered

forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered, 
  method = "REML",
  study_labels = ordered_by_year[1:328, "code"], # include study name label
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative")

forest_plot_CMA
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/CMA_all_case_studies.pdf")
forest_plot_CMA
dev.off()
dev.off()
