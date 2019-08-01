## LOAD PACKAGES ####
library(metafor)
library(metaviz)
library(ggplot2)
library(lme4)
library(lmerTest) # lme4 doesn't give p-values, but this package adds them
library(cowplot)

# Read in data
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

# Get effect sizes for all studies in the database
# The output here, or the ROM, is the log-transformed ratio of means
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)

#### using the lme4 tutorials combined with the iacarella method
head(effect_sizes_richness_imputed)

# Add in the FIXED EFFECTS predictor of year
model1 <- lmer(yi~1 + publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model1)
View(effect_sizes_richness_imputed)
decline_overall_plot <- ggplot(data = effect_sizes_richness_imputed, 
                              aes(
                                x = publicationyear,
                                y = yi
                              )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = .8,
             alpha = .8,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Publication year") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 14)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=.3)
decline_overall_plot

