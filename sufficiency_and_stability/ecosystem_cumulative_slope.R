## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)

# Calculate effect size
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)

ordered_by_year <- arrange(effect_sizes_richness_imputed, publicationyear)
head(ordered_by_year)


# Forests
effects_forest <- filter(ordered_by_year, ecosystemforheatmap == "forest")
effects_forest

rma_forest <- rma(yi=effects_forest$yi, 
                  vi=effects_forest$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_forest)

cma_forest <- viz_forest(x = rma_forest, 
                         #study_labels = effects_forest[, "publicationyear"], 
                         study_labels = forest_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         # variant = "thick",
                         type = "cumulative") +
  ggtitle("Forest (N = 75)") +
  theme(plot.title = element_text(hjust=0.5))

# Prep the for loop
effects_forest$cumulative_slope <- rep(NA, length(effects_forest$code))
effects_forest

cma_forest$data$order <- seq(1:75)
cma_forest$data
for (i in 1:75)
{
  temp_df <- lm(x ~ order, data = cma_forest$data[1:i,])
  effects_forest$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_forest
tail(effects_forest)

which(abs(effects_forest$cumulative_slope)< .005)

all_forests <- lm(x ~ order, data= cma_forest$data)
summary(all_forests)
