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

#fail safe number
length(ordered_by_year$code)
fail_safe_overall <- fsn(ordered_by_year$yi, ordered_by_year$vi, type = "Rosenthal")

fail_safe_overall
(fail_safe_overall$fsnum)/(5*length(ordered_by_year$yi))

# This works for just one row
fail_safe_one <- fsn(ordered_by_year[1:1,]$yi, ordered_by_year[1:1,]$vi, type = "Rosenthal")
fail_safe_one$fsnum
fail_safe_ratio <- (fail_safe_one$fsnum)/(5*length(ordered_by_year[1:1,]$yi)+10)
fail_safe_ratio

# calculate failsafe ratio for all studies
ordered_by_year$failsaferatio <- rep(NA, 334)
for(i in 1:334)
{
  temp_df <- fsn(ordered_by_year[1:i,]$yi, ordered_by_year[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(ordered_by_year[1:i,]$yi)+10)
  ordered_by_year$failsaferatio[i] <- failsafe_one_run
  
}

ordered_by_year$order <- seq(1:334)
head(ordered_by_year)
length(counted_all_pubs)
counted_all_pubs$x

gg <- ggplot(ordered_by_year, aes(x = failsaferatio, y = order)) + geom_point()
gg
gg <- gg + scale_y_continuous(trans = "reverse")
gg <- gg + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg <- gg + xlab("Failsafe ratio") +
  ylab("Publication order")
gg <- gg + theme_bw()
gg <- gg + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank())
gg

# Having trouble with adding axes
gg <- gg + scale_y_continuous(trans = "reverse", labels = counted_all_pubs$x, breaks = c(0,6,8, 16,22, 33, 5,7,14,19,29,35,45,58,79,99,134,154,173,200,222,259,281,324))
