## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(lme4)
library(lmerTest) # lme4 doesn't give p-values, but this package adds them
library(metafor)
library(cowplot)

# Get effect sizes for all studies in the database
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)

rma_pubyear_and_sample_size <- lmer(yi~1 + publicationyear*total_sample_size + (1|code), data = effect_sizes_richness_imputed)
summary(rma_pubyear_and_sample_size)

sample_size_bubble_plot <- ggplot(
  effect_sizes_richness_imputed,
  aes(x = publicationyear, y = yi, size = total_sample_size)
) +
  geom_point(alpha = 0.4,
             position = "jitter",
             color = "#238A8DFF") +
  scale_colour_continuous(guide = FALSE) +
  xlab("Publication year") +
  ylab("ln(Response ratio)") +
  geom_smooth(method = "lm", se = FALSE,
              color = "#453781FF") +
  geom_hline(
    yintercept = 0,
    col = "black",
    linetype = "dashed",
    alpha = .5
  ) +
  theme_cowplot() +
  theme(text = element_text(size = 20),
        legend.position = "none")
sample_size_bubble_plot
