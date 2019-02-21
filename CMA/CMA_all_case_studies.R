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

# Make study labels
counted_all_pubs <- plyr::count(ordered_by_year$publicationyear)
counted_all_pubs
soverall_CMA_study_labels <- c(1999, 
                              strrep("", 1:4), 
                              2000, 
                              strrep("",1),
                              2001,
                              strrep("",1:6),
                              2002,
                              strrep("",1:4),
                              2003,
                              strrep("",1:9),
                              2004,
                              strrep("",1:15),
                              2005,
                              strrep("",1:9),
                              2006,
                              strrep("",1:12),
                              2007,
                              strrep("",1:20),
                              2008,
                              strrep("",1:19),
                              2009,
                              strrep("",1:34),
                              2010,
                              strrep("",1:19),
                              2011,
                              strrep("",1:18),
                              2012,
                              strrep("",1:26),
                              2013,
                              strrep("",1:21),
                              2014,
                              strrep("",1:36),
                              2015,
                              strrep("",1:21),
                              2016,
                              strrep("",1:42))

#make forest plot
forest_plot_CMA <- viz_forest(
  x = random_effects_model_ordered, 
  method = "REML",
  #study_labels = ordered_by_year[1:334, "code"], # include study name label
  study_labels = overall_CMA_study_labels, # include custom study labels that skip any repeated years
  xlab = "Ratio of Means", # make a label along x-axis for effect size
  col = "Blues",
  type = "cumulative",
  text_size = 3)
forest_plot_CMA

pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/CMA_all_case_studies.pdf")
forest_plot_CMA
dev.off()
dev.off()



# data for first 5 effects

# can look at exact data that forest plot is made with
forest_plot_CMA$data
first_five_effect <- c(-1.04112114,-1.0883960,-1.1379484,-0.9699270,-0.7790654)
first_five_low_ci <- c(-1.5198541,-1.5157978,-1.5438819,-1.3795680,-1.2429049)
first_five_high_ci <- c(-0.56238815,-0.66099414,-0.73201492,-0.56028606,-0.31522604)
mean(first_five_effect)
sd(first_five_effect)
exp(-1.003292)
1-0.3666704 # this is richness decline
mean(first_five_low_ci)
mean(first_five_high_ci)

-1.003292 - 1.440401
-1.003292 + 0.5661819

##
cumul(random_effects_model_ordered)
