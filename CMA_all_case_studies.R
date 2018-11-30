## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

# Calculate effect size
effect_sizes_richness <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)

## Run a cumulative MA
ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year)

cma_all_studies <- viz_forest(x = ordered_by_year[, c("yi", "vi")], 
           #group = effect_sizes_richness[, "invasivespeciestaxa"], 
           study_labels = ordered_by_year[, "code"], 
           xlab = "Hedge's d",
           variant = "thick",
           type = "cumulative")
cma_all_studies

pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/CMA_all_case_studies.pdf")
cma_all_studies
dev.off()
dev.off()
