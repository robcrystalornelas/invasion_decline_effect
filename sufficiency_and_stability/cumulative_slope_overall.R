## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

# Calculate effect size
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data_imputed$mean_invaded,       
                                n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data_imputed$SD_invaded, 
                                m2i = raw_data_imputed$mean_control,
                                n2i = raw_data_imputed$sample_size_control, 
                                sd2i = raw_data_imputed$SD_control,
                                data = raw_data_imputed)
head(effect_sizes_richness)

## Run a cumulative MA
## first, order studies by year
ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year, N = 5)
ordered_by_year
dim(ordered_by_year)
# Random effects model
random_effects_model_ordered <- rma(yi=ordered_by_year$yi, 
                                    vi=ordered_by_year$vi,
                                    method = "REML",
                                    test = "knha",
                                    data=ordered_by_year)
random_effects_model_ordered

forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered, 
  method = "REML",
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative",
  text_size = 3)
forest_plot_CMA

# calculating slopes of CMA results
forest_plot_CMA$data[1:5,]

ordered_by_year$cumulative_slope <- rep(NA, 334)
ordered_by_year

for (i in 1:334)
{
  temp_df <- lm(x ~ labels, data =forest_plot_CMA$data[1:i,])
  ordered_by_year$cumulative_slope[i] <- temp_df$coefficients[2]
}
ordered_by_year
ordered_by_year$cumulative_slope
which((abs(ordered_by_year$cumulative_slope) < .005))
ordered_by_year[69,]
ordered_by_year$order <- seq(1:334)
ordered_by_year

# Stabilization plot
gg <- ggplot(ordered_by_year, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
gg <- gg + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg <- gg + scale_y_continuous(trans = "reverse", labels = counted_all_pubs$x, breaks = c(1,6,8,15,20,30,46,56,69,90,110,145,165,184,211,233,270,292))
gg
gg <- gg + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
gg <- gg + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
gg <- gg + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
gg <- gg + xlab("Cumulative slopes") +
  ylab("Publication year")
gg <- gg + theme_bw()
gg <- gg + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none")
gg


# Examples
linear_model_first_five <- lm(x ~ labels, data =forest_plot_CMA$data[1:5,])
linear_model_first_five$coefficients[2]
linear_model_all_studies <- lm(x ~ labels, data = forest_plot_CMA$data)
linear_model_all_studies$coefficients
first_five <- ggplot(forest_plot_CMA$data, aes(x=labels, y=x)) + 
  # geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_jitter(shape = 1)
first_five