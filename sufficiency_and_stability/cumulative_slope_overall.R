## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(cowplot)

# Calculate effect size
effect_sizes_richness <-
  escalc(
    "ROM",
    # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
    m1i = raw_data_imputed$mean_invaded,
    n1i = raw_data_imputed$sample_size_invaded,
    # Then, follow with all of the columns needed to compute SMD
    sd1i = raw_data_imputed$SD_invaded,
    m2i = raw_data_imputed$mean_control,
    n2i = raw_data_imputed$sample_size_control,
    sd2i = raw_data_imputed$SD_control,
    data = raw_data_imputed
  )
head(effect_sizes_richness)

## Run a cumulative MA
## first, order studies by year
ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year, N = 5)
ordered_by_year
dim(ordered_by_year)
# Random effects model
random_effects_model_ordered <- rma(
  yi = ordered_by_year$yi,
  vi = ordered_by_year$vi,
  method = "REML",
  test = "knha",
  data = ordered_by_year
)
random_effects_model_ordered

forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered,
  method = "REML",
  xlab = "Ratio of Means",
  # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative",
  text_size = 3
)
# forest_plot_CMA

# calculating slopes of CMA results
forest_plot_CMA$data[1:5, ]

ordered_by_year$cumulative_slope <- rep(NA, 334)
ordered_by_year

for (i in 1:334)
{
  temp_df <- lm(x ~ labels, data = forest_plot_CMA$data[1:i, ])
  ordered_by_year$cumulative_slope[i] <- temp_df$coefficients[2]
}
ordered_by_year
ordered_by_year$cumulative_slope
which((abs(ordered_by_year$cumulative_slope) < .005))
ordered_by_year[69, ]
ordered_by_year$order <- seq(1:334)
ordered_by_year

# Stabilization plot
counted_all_pubs
overall_CMA_study_labels_legible

counted_all_pubs
counted_all_pubs_legible <- counted_all_pubs[-c(2), ]
counted_all_pubs_legible

stabilization_full <-
  ggplot(ordered_by_year, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 &
                                                                                   cumulative_slope > -.005))
stabilization_full <-
  stabilization_full + scale_colour_manual(values = setNames(c('red', 'black'), c(T, F)))
stabilization_full <-
  stabilization_full + scale_y_continuous(
    trans = "reverse",
    labels = counted_all_pubs_legible$x,
    breaks = which(overall_CMA_study_labels_legible != "")
  )
stabilization_full
stabilization_full <-
  stabilization_full + geom_vline(
    xintercept = 0.005,
    colour = "red",
    size = .5,
    linetype = 2
  )
stabilization_full <-
  stabilization_full + geom_vline(
    xintercept = 0.005,
    colour = "red",
    size = .5,
    linetype = 2
  )
stabilization_full <-
  stabilization_full + geom_vline(
    xintercept = -.005,
    colour = "red",
    size = .5,
    linetype = 2
  )
stabilization_full <- stabilization_full + xlab("Cumulative slopes") +
  ylab("Publication year")
stabilization_full <- stabilization_full + theme_bw()
stabilization_full <- stabilization_full + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5, size = 14),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 14)
)
stabilization_full
