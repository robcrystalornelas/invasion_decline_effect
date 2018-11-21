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

# Now, subset effect sizes into richness types
effect_size_native <- filter(effect_sizes_no_outlier, richness_of_all_or_native == "native") %>%
  arrange(publicationyear)

effect_size_introduced <- filter(effect_sizes_no_outlier, richness_of_all_or_native == "introduced") %>%
  arrange(publicationyear)

effect_size_no_distinction <- filter(effect_sizes_no_outlier, richness_of_all_or_native == "no distinction") %>%
  arrange(publicationyear)

# Make CMAs for different richness groups
cma_introduced <- viz_forest(x = effect_size_introduced[, c("yi", "vi")], 
                             group = effect_size_introduced[, "richness_of_all_or_native"], 
                             study_labels = effect_size_introduced[, "code"], 
                             summary_label = c("Summary:introduced"), 
                             xlab = "Cohen d",
                             variant = "thick",
                             col = "Greys",
                             text_size = 8,
                             type = "cumulative")
cma_introduced

cma_native <- viz_forest(x = effect_size_native[, c("yi", "vi")], 
                         group = effect_size_native[, "richness_of_all_or_native"], 
                         study_labels = effect_size_native[, "code"], 
                         summary_label = c("Summary: native"), 
                         xlab = "Cohen d",
                         variant = "thick",
                         col = "Greys",
                         type = "cumulative")
cma_native

cma_no_distinction <- viz_forest(x = effect_size_no_distinction[, c("yi", "vi")], 
                               group = effect_size_no_distinction[, "richness_of_all_or_native"], 
                               study_labels = effect_size_no_distinction[, "code"], 
                               summary_label = c("Summary: no distinction"), 
                               xlab = "Cohen d",
                               variant = "thick",
                               col = "Greys",
                               type = "cumulative")
cma_no_distinction
