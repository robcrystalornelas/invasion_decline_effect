## Load libraries ####
library(metafor)
library(metaviz)
library(ggplot2)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Calculate effect sizes
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)

## Build linear model
linear_model_imputed <- lm(yi ~ publicationyear, data=effect_sizes_richness_imputed)  # build linear regression model on full data
print(linear_model_imputed)
# In equation form, effect size = intercept + publication year

# first, have to check if it's statistically significant
summary(linear_model_imputed)
# it is significant

# basic linear regression plot
linear_model_imputed_gg <- ggplot(effect_sizes_richness_imputed, aes(x=publicationyear, y=yi)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
linear_model_imputed_gg
linear_model_imputed_gg <- linear_model_imputed_gg + scale_x_continuous(name = "Publication Year") +
  scale_y_continuous(name = "Effect size")
linear_model_imputed_gg <- linear_model_imputed_gg + theme_bw() + theme(text = element_text(size = 20))
linear_model_imputed_gg

linear_model_imputed_gg
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/linear_model_publication_year_imputed.pdf")
linear_model_imputed_gg
dev.off()
dev.off()
