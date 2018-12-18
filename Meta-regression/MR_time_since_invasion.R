## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)

## ORGANIZE DATA ####
head(raw_data)
temporal_raw <- select(raw_data_imputed,lastname,publicationyear,firstyeardetected,firstyearatsite,firstyearoverall,yearbegins,yearends,studylength,mean_control, SD_control, sample_size_control, mean_invaded,SD_invaded,sample_size_invaded)

# Make new row w/ time since introduction
temporal_raw <- mutate(temporal_raw, time_since_invasion = yearbegins - firstyearoverall)
class(temporal_raw$time_since_invasion)

# Make bins so that they align with strayer categories
temporal_raw$time_since_invasion <- cut(temporal_raw$time_since_invasion, breaks = c(0,3,10,30,100,1000), labels = c("<3","3.1-10","10.1-30","30.1-100","100+"))
head(temporal_raw)
temporal_raw
# Make sure NAs are included in the dataset
levels(temporal_raw$time_since_invasion)
temporal_raw$time_since_invasion = factor(temporal_raw$time_since_invasion, levels=c(levels(temporal_raw$time_since_invasion), 00))
temporal_raw$time_since_invasion[is.na(temporal_raw$time_since_invasion)] = 00
levels(temporal_raw$time_since_invasion)

# Calculate effect sizes
effect_sizes_richness <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = temporal_raw$mean_invaded,       
                                n1i = temporal_raw$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = temporal_raw$SD_invaded, 
                                m2i = temporal_raw$mean_control,
                                n2i = temporal_raw$sample_size_control, 
                                sd2i = temporal_raw$SD_control,
                                data = temporal_raw)


# Meta-regression for study length
mixed_effects_time_since_invasion <- rma(yi, # outcome
                                  vi, # measure of variance
                                  mods = ~ time_since_invasion - 1, # multiple moderating variables modeled as main effects
                                  method = "REML",
                                  data = effect_sizes_richness,
                                  slab = paste(lastname, publicationyear, sep = ""))
mixed_effects_time_since_invasion

# How many case studies fall into each time category
sample_size_table <- effect_sizes_richness %>%
  group_by(time_since_invasion) %>%
  summarise(no_rows = length(time_since_invasion)) %>%
  rename(`time since invasion` = time_since_invasion) %>%
  rename(`sample size` = no_rows)
sample_size_table
levels(sample_size_table$`time since invasion`)[levels(sample_size_table$`time since invasion`)=="0"] <- "NA"

# Make forest plot
forest_plot_time_since_invasion <- viz_forest(x = mixed_effects_time_since_invasion, 
                                       method = "REML",
                                       type = "summary_only",
                                       summary_label = c("< 3 (N = 2)","3.1-10 (N = 13)","10.1-30 (N = 40)","30.1-100 (N = 49)","100+ (N = 63)","NA (N = 150)"), 
                                       xlab = "ratio of means",
                                       col = "black",
                                       variant = "thick",
                                       table_headers = c("time since invasion","sample size"),
                                       text_size = 7,
                                       summary_table = sample_size_table,
                                       annotate_CI = TRUE)

forest_plot_time_since_invasion
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/forest_plot_MR_time_since_invasion.pdf", width = 20, height = 8)
forest_plot_time_since_invasion
dev.off()
dev.off()
