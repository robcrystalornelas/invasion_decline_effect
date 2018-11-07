## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Clean data ####
dim(raw_data)
tail(raw_data)

## Analyze data ####
# Calculate effect size
head(raw_data)
effect_sizes_richness <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)
effect_sizes_richness

# Fixed effects model
fixed_effect_model_results <- rma(yi, # outcome
                                  vi, # measure of variance
                                  method = "FE",
                                  data = effect_sizes_richness,
                                  slab = paste(lastname, publicationyear, sep = ""))
fixed_effect_model_results
