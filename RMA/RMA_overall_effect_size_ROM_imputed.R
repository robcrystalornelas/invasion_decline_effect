## Load libraries ####
library(metafor)
library(metaviz)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Clean data ####
dim(raw_data_imputed)
unique(raw_data_imputed$code)
## Analyze data ####
# Effect size is Log transformed ratio of means
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)

# Random effects model
head(effect_sizes_richness_imputed)
dim(effect_sizes_richness_imputed)
random_effects_model_imputed <- rma(yi=effect_sizes_richness_imputed$yi, 
                            vi=effect_sizes_richness_imputed$vi,
                            method = "REML",
                            test = "knha",
                            data=effect_sizes_richness_imputed)
random_effects_model_imputed

# Random effects model forest plot
forest_plot_random_effects_imputed <- viz_forest(
  x = random_effects_model_imputed, 
  method = "REML",
  study_labels = effect_sizes_richness_imputed[1:335, "code"], # include study name label
  xlab = "Response Ratio", # make a label along x-axis for effect size
  col = "Blues"
  #  variant = "thick"
)
forest_plot_random_effects_imputed

# Make a funnel plot
head(effect_sizes_richness_imputed)
# but first, have to log transform yi and vi columns


head(effect_sizes_richness_imputed)
effect_sizes_richness_imputed <- mutate(effect_sizes_richness_imputed, log_rr = log(yi + 6.3))
effect_sizes_richness_imputed <- mutate(effect_sizes_richness_imputed, log_rr_se = log(vi+ 1))

head(effect_sizes_richness_imputed)
min(effect_sizes_richness_imputed$log_rr_se)

# Funnel plot
funnel(random_effects_model_imputed)

# trim and fill
tf1 <- trimfill(random_effects_model_imputed)
print(tf1, digits = 2, comb.fixed = TRUE)

# Eggers regression
regtest.rma(random_effects_model_imputed, model = "rma", predictor = "vi")

# fail safe number
fsn(yi, vi, data = effect_sizes_richness_imputed)
