## Load libraries ####
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Calculate effect sizes
head(raw_data)
effect_sizes_richness <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)
max(effect_sizes_richness$yi)
min(effect_sizes_richness$yi)
which.min(effect_sizes_richness$yi) # this is row number of the extreme minimum value

# Remove row with extreme minimum
effect_sizes_no_outlier <- effect_sizes_richness[-c(106), ]
min(effect_sizes_no_outlier$yi)

# Make FULL CMA plot
head(effect_sizes_no_outlier)
cma_forest_plot<- viz_forest(x = effect_sizes_no_outlier[, c("yi", "vi")], 
           group = effect_sizes_no_outlier[, "richness_of_all_or_native"], 
           study_labels = effect_sizes_no_outlier[, "code"], 
           summary_label = c("exotic", "native","no distinction"), 
           xlab = "Cohen d",
           variant = "thick",
           type = "cumulative")
cma_forest_plot
