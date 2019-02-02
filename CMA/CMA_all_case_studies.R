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
ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year, N = 5)
ordered_by_year
# Random effects model
random_effects_model_ordered <- rma(yi=ordered_by_year$yi, 
                            vi=ordered_by_year$vi,
                            method = "REML",
                            test = "knha",
                            data=ordered_by_year)
random_effects_model_ordered

forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered, 
  method = "REML",
  study_labels = ordered_by_year[1:335, "code"], # include study name label
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative")

forest_plot_CMA
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/CMA_all_case_studies.pdf")
forest_plot_CMA
dev.off()
dev.off()

# data for first 5 years
first_five_effect <- c(-1.04112114,-1.27406033,-1.59435985,-0.34484049,-0.30833840,-0.08770558)
first_five_ci <- c(0.059661001,0.234309161,0.437993868,0.160824259,0.037811592,0.007000601)
mean(first_five_effect)
mean(first_five_ci)/2
exp(-.775071)
1-0.4606711
-.775071 + 0.07813337
-.775071 - 0.07813337
