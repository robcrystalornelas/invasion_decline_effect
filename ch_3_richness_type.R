## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

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

# Meta-regression model
head(effect_sizes_richness)
mixed_effects_richness_type <- rma(yi, # outcome
                                      vi, # measure of variance
                                      mods = ~ richness_of_all_or_native - 1, # multiple moderating variables modeled as main effects
                                      method = "REML",
                                      data = effect_sizes_richness,
                                      slab = paste(lastname, publicationyear, sep = ""))
mixed_effects_richness_type

# Make forest plot
forest_plot_richness_type <- viz_forest(x = mixed_effects_richness_type, 
                                     method = "REML",
                                     # group = "Nesting Location",
                                     type = "summary_only",
                                     summary_label = c("Exotic","Native","No distinction"), 
                                     xlab = "Hedge's d",
                                     col = "black",
                                     variant = "thick",
                                     text_size = 7,
                                     annotate_CI = TRUE
)
forest_plot_richness_type

pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/ch_3_MR_richness_type.pdf")
forest_plot_richness_type
dev.off()
dev.off()
