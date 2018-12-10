## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

## ORGANIZE DATA ####
head(raw_data)
temporal_raw <- select(raw_data,lastname,publicationyear,firstyeardetected,firstyearatsite,yearbegins,yearends,studylength,mean_control, SD_control, sample_size_control, mean_invaded,SD_invaded,sample_size_invaded)

# R doesn't know what to do with <1 factor, so replace with numerical placeholder
temporal_raw$studylength <- as.character(temporal_raw$studylength) # first make it a character vector
temporal_raw$studylength[temporal_raw$studylength == "<1"] <- "0.5" # then replace <1 with a placeholder of .5
head(temporal_raw)
temporal_raw$studylength <- as.numeric(temporal_raw$studylength) # now conver the study length column back to numeric
temporal_raw # check to make sure all numbers remained

# Make bins so that they align with strayer categories
temporal_raw$studylengthbinned <- cut(temporal_raw$studylength, breaks = c(0,1,3,10,250), labels = c("0-1","1.1-3","3.1-10",">10"))
head(temporal_raw)

# Make sure NAs are included in the dataset
levels(temporal_raw$studylengthbinned)
temporal_raw$studylengthbinned = factor(temporal_raw$studylengthbinned, levels=c(levels(temporal_raw$studylengthbinned), 00))
temporal_raw$studylengthbinned[is.na(temporal_raw$studylengthbinned)] = 00
levels(temporal_raw$studylengthbinned)

# Calculate effect sizes
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = temporal_raw$mean_invaded,       
                                n1i = temporal_raw$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = temporal_raw$SD_invaded, 
                                m2i = temporal_raw$mean_control,
                                n2i = temporal_raw$sample_size_control, 
                                sd2i = temporal_raw$SD_control,
                                data = temporal_raw)
effect_sizes_richness %>%
group_by(studylengthbinned) %>%
  summarise(no_rows = length(studylengthbinned))

# Meta-regression for study length
mixed_effects_study_length <- rma(yi, # outcome
                                   vi, # measure of variance
                                   mods = ~ studylengthbinned - 1, # multiple moderating variables modeled as main effects
                                   method = "REML",
                                   data = effect_sizes_richness,
                                   slab = paste(lastname, publicationyear, sep = ""))
mixed_effects_study_length

sample_size_table_study_length <- effect_sizes_richness %>%
  group_by(studylengthbinned) %>%
  summarise(no_rows = length(studylengthbinned)) %>%
  rename(`study length` = studylengthbinned) %>%
  rename(`sample size` = no_rows)

# Replace 00 (the na placeholder), with NA as a label
levels(sample_size_table_study_length$`study length`)[levels(sample_size_table_study_length$`study length`)=="0"] <- "NA"

# Make forest plot
forest_plot_study_length <- viz_forest(x = mixed_effects_study_length, 
                                        method = "REML",
                                        type = "summary_only",
                                        xlab = "ratio of means",
                                        col = "black",
                                        variant = "thick",
                                        summary_table = sample_size_table_study_length,
                                        table_headers = c("study length","sample size"),
                                        text_size = 7,
                                        annotate_CI = TRUE)

forest_plot_study_length
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/forest_plot_MR_study_length.pdf", width = 20, height = 8)
forest_plot_study_length
dev.off()
dev.off()

