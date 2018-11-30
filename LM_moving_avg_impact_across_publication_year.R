## Load libraries ####

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

library(metafor)
library(zoo)
library(tidyverse)      # data manipulation and visualization
library(lubridate)      # easily work with dates and times
library(fpp2)           # working with time series data

## Calculate effect sizes
head(raw_data)
effect_sizes_richness <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)


effect_size_tibble <- as.tibble(effect_sizes_richness)
effect_size_tibble$publicationyear <- as.Date(effect_size_tibble$publicationyear)
head(effect_size_tibble)

rolling_avg <- effect_size_tibble %>%
  select(publicationyear, effect_size = yi) %>%
  mutate(av_1999 = rollmean(effect_size, k = 1, fill = NA),
         av_2000 = rollmean(effect_size, k = 2, fill = NA),
         av_2001 = rollmean(effect_size, k = 3, fill = NA),
         av_2002 = rollmean(effect_size, k = 4, fill = NA),
         av_2003 = rollmean(effect_size, k = 5, fill = NA),
         av_2004 = rollmean(effect_size, k = 6, fill = NA),
         av_2005 = rollmean(effect_size, k = 7, fill = NA),
         av_2006 = rollmean(effect_size, k = 8, fill = NA),
         av_2007 = rollmean(effect_size, k = 9, fill = NA),
         av_2008 = rollmean(effect_size, k = 10, fill = NA),
         av_2009 = rollmean(effect_size, k = 11, fill = NA),
         av_2010 = rollmean(effect_size, k = 12, fill = NA),
         av_2011 = rollmean(effect_size, k = 13, fill = NA),
         av_2012 = rollmean(effect_size, k = 14, fill = NA),
         av_2013 = rollmean(effect_size, k = 15, fill = NA),
         av_2014 = rollmean(effect_size, k = 16, fill = NA),
         av_2015 = rollmean(effect_size, k = 17, fill = NA),
         av_2016 = rollmean(effect_size, k = 18, fill = NA))
class(rolling_avg)
rollmean(effect_size, k = 1, fill = NA)

# Make a graph
head(rolling_avg)
rolling_avg %>%
  gather(metric, value, effect_size:av_2016) %>%
  ggplot(aes(publicationyear, value, color = metric)) +
  geom_line()


## ECON EXAMPLE
savings <- economics %>%
  select(date, srate = psavert) %>%
  mutate(srate_ma01 = rollmean(srate, k = 1, fill = NA),
         srate_ma02 = rollmean(srate, k = 2, fill = NA),
         srate_ma03 = rollmean(srate, k = 3, fill = NA),
         srate_ma05 = rollmean(srate, k = 4, fill = NA),
         srate_ma10 = rollmean(srate, k = 5, fill = NA),
         srate_ma10 = rollmean(srate, k = 5, fill = NA))
head(savings)
class(savings)

savings %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line()
