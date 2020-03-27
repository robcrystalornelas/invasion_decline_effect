## READ IN DATA ####
source("~/Desktop/research/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

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
1 - exp(-.2312)

# Make study labels
counted_all_pubs <- plyr::count(ordered_by_year$publicationyear)
counted_all_pubs
overall_CMA_study_labels_legible <- c(
  1999,
  strrep("", 1:6),
  2001,
  strrep("", 1:6),
  2002,
  strrep("", 1:4),
  2003,
  strrep("", 1:9),
  2004,
  strrep("", 1:15),
  2005,
  strrep("", 1:9),
  2006,
  strrep("", 1:12),
  2007,
  strrep("", 1:20),
  2008,
  strrep("", 1:19),
  2009,
  strrep("", 1:34),
  2010,
  strrep("", 1:19),
  2011,
  strrep("", 1:18),
  2012,
  strrep("", 1:26),
  2013,
  strrep("", 1:21),
  2014,
  strrep("", 1:36),
  2015,
  strrep("", 1:21),
  2016,
  strrep("", 1:42)
)

length(overall_CMA_study_labels_legible)
overall_CMA_study_labels_legible
reversed_overall_CMA_study_labels_legible <- rev(overall_CMA_study_labels_legible)

#make forest plot
forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered,
  method = "REML",
  xlab = "ln(Response Ratio)",
  col = "Reds",
  type = "cumulative",
  text_size = 6)
forest_plot_CMA

counted_all_pubs
counted_all_pubs_legible <- counted_all_pubs[-c(2), ]
counted_all_pubs_legible
counted_all_pubs_legible$x
rev(counted_all_pubs_legible$x)

forest_plot_CMA <- forest_plot_CMA + scale_y_continuous(
  labels = rev(counted_all_pubs_legible$x),
  breaks = which(rev(overall_CMA_study_labels_legible) != ""))
forest_plot_CMA
forest_plot_CMA <- forest_plot_CMA + ylab("Publication year")
forest_plot_CMA

# data for first 5 effects
# can look at exact data that forest plot is made with
forest_plot_CMA$data
ordered_by_year[1:5, ]
random_effects_model_ordered_first_five <-
  rma(
    yi = ordered_by_year[1:5, ]$yi,
    vi = ordered_by_year[1:5, ]$vi,
    method = "REML",
    test = "knha",
    data = ordered_by_year[1:5, ]
  )
random_effects_model_ordered_first_five

dev.off()
1 - exp(-.7791) * ((sigma(
  random_effects_model_ordered_first_five
) ^ 2) / 2)

### Combining all 3 summary plots
plot_grid(
  forest_plot_CMA,
  suff_full,
  stabilization_full,
  ncol = 3,
  labels = c("A", "B", "C"),
  align = 'h'
)



