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

# Random effects model
random_effects_model_results <- rma(yi = effect_sizes_richness$yi, # Outcome variable
                                        vi = effect_sizes_richness$vi, # variances
                                        method = "REML") # REML is common estimator

print(random_effects_model_results, digits=5)

# Random effects model forest plot
forest_plot_random_effects <- viz_forest(
  x = random_effects_model_results, 
  method = "REML",
  # type = "summary_only",
  # study_labels = random_effects_abundance_results[1:131, "unique_id"], 
  xlab = "Hedge's d",
  col = "Blues"
  #variant = "thick"
)

forest_plot_random_effects
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/forest_plot_random_effects.pdf")
forest_plot_random_effects
dev.off()
dev.off()

# Funnel plot
funnel(random_effects_model_results, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0)

# Trim and fill
tf1 <- trimfill(random_effects_model_results)
print(tf1, digits = 2, comb.fixed = TRUE)

