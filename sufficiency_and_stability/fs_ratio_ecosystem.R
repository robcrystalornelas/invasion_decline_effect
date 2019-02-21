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
levels(ordered_by_year$ecosystemforheatmap)

# Only do CMAs on taxa with more than 10 cases ####
plyr::count(ordered_by_year$ecosystemforheatmap)

# Forest ratio
effects_forest <- filter(ordered_by_year, ecosystemforheatmap == "forest")
effects_forest

effects_forest$fsr <- rep(NA, length(effects_forest$code))
effects_forest
for(i in 1:length(effects_forest$code))
{
  temp_df <- fsn(effects_forest[1:i,]$yi, effects_forest[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_forest[1:i,]$yi)+10)
  effects_forest$failsaferatio[i] <- failsafe_one_run
  
}
effects_forest
effects_forest$failsaferatio
effects_forest$order <- seq(1:75)

# Forest graph
gg_forest <- ggplot(effects_forest, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_forest
gg_forest <- gg_forest + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_forest
gg_forest <- gg_forest + scale_y_continuous(trans = "reverse")
gg_forest
gg_forest <- gg_forest + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_forest
gg_forest <- gg_forest + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Forest (N = 75)")
gg_forest
gg_forest <- gg_forest + theme_bw()
gg_forest <- gg_forest + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_forest

################
effects_lotic <- filter(ordered_by_year, ecosystemforheatmap == "lotic")

effects_lotic$fsr <- rep(NA, length(effects_lotic$code))
effects_lotic
for(i in 1:length(effects_lotic$code))
{
  temp_df <- fsn(effects_lotic[1:i,]$yi, effects_lotic[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_lotic[1:i,]$yi)+10)
  effects_lotic$failsaferatio[i] <- failsafe_one_run
  
}
effects_lotic
effects_lotic$failsaferatio
effects_lotic$order <- seq(1:46)

# Algae graph
gg_lotic <- ggplot(effects_lotic, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_lotic
gg_lotic <- gg_lotic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_lotic
gg_lotic <- gg_lotic + scale_y_continuous(trans = "reverse")
gg_lotic
gg_lotic <- gg_lotic + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_lotic
gg_lotic <- gg_lotic + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Lotic (N = 46)")
gg_lotic
gg_lotic <- gg_lotic + theme_bw()
gg_lotic <- gg_lotic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_lotic

################
effects_grassland <- filter(ordered_by_year, ecosystemforheatmap == "grassland")

effects_grassland$fsr <- rep(NA, length(effects_grassland$code))
effects_grassland
for(i in 1:length(effects_grassland$code))
{
  temp_df <- fsn(effects_grassland[1:i,]$yi, effects_grassland[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_grassland[1:i,]$yi)+10)
  effects_grassland$failsaferatio[i] <- failsafe_one_run
  
}
effects_grassland
effects_grassland$failsaferatio
effects_grassland$order <- seq(1:43)

# Grassland graph
gg_grass <- ggplot(effects_grassland, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_grass
gg_grass <- gg_grass + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_grass
gg_grass <- gg_grass + scale_y_continuous(trans = "reverse")
gg_grass
gg_grass <- gg_grass + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_grass
gg_grass <- gg_grass + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Grassland (N = 43)")
gg_grass
gg_grass <- gg_grass + theme_bw()
gg_grass <- gg_grass + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_grass

################
effects_island <- filter(ordered_by_year, ecosystemforheatmap == "island")

effects_island$fsr <- rep(NA, length(effects_island$code))
effects_island
for(i in 1:length(effects_island$code))
{
  temp_df <- fsn(effects_island[1:i,]$yi, effects_island[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_island[1:i,]$yi)+10)
  effects_island$failsaferatio[i] <- failsafe_one_run
  
}
effects_island
effects_island$failsaferatio
effects_island$order <- seq(1:35)

# Algae graph
gg_island <- ggplot(effects_island, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_island
gg_island <- gg_island + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_island
gg_island <- gg_island + scale_y_continuous(trans = "reverse")
gg_island
gg_island <- gg_island + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_island
gg_island <- gg_island + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Island (N = 35)")
gg_island
gg_island <- gg_island + theme_bw()
gg_island <- gg_island + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_island

################
effects_estuarine <- filter(ordered_by_year, ecosystemforheatmap == "estuarine")

effects_estuarine$fsr <- rep(NA, length(effects_estuarine$code))
effects_estuarine
for(i in 1:length(effects_estuarine$code))
{
  temp_df <- fsn(effects_estuarine[1:i,]$yi, effects_estuarine[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_estuarine[1:i,]$yi)+10)
  effects_estuarine$failsaferatio[i] <- failsafe_one_run
  
}
effects_estuarine
effects_estuarine$failsaferatio
effects_estuarine$order <- seq(1:34)

# Algae graph

gg_estuarine <- ggplot(effects_estuarine, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_estuarine
gg_estuarine <- gg_estuarine + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_estuarine
gg_estuarine <- gg_estuarine + scale_y_continuous(trans = "reverse")
gg_estuarine
gg_estuarine <- gg_estuarine + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_estuarine
gg_estuarine <- gg_estuarine + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Estuarine (N = 34)")
gg_estuarine
gg_estuarine <- gg_estuarine + theme_bw()
gg_estuarine <- gg_estuarine + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_estuarine

################
effects_lentic <- filter(ordered_by_year, ecosystemforheatmap == "lentic")

effects_lentic$fsr <- rep(NA, length(effects_lentic$code))
effects_lentic
for(i in 1:length(effects_lentic$code))
{
  temp_df <- fsn(effects_lentic[1:i,]$yi, effects_lentic[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_lentic[1:i,]$yi)+10)
  effects_lentic$failsaferatio[i] <- failsafe_one_run
  
}
effects_lentic
effects_lentic$failsaferatio
effects_lentic$order <- seq(1:30)

# Algae graph
gg_lentic <- ggplot(effects_lentic, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_lentic
gg_lentic <- gg_lentic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_lentic
gg_lentic <- gg_lentic + scale_y_continuous(trans = "reverse")
gg_lentic
gg_lentic <- gg_lentic + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_lentic
gg_lentic <- gg_lentic + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Lentic (N = 30)")
gg_lentic
gg_lentic <- gg_lentic + theme_bw()
gg_lentic <- gg_lentic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_lentic

################
effects_coastal <- filter(ordered_by_year, ecosystemforheatmap == "coastal")

effects_coastal$fsr <- rep(NA, length(effects_coastal$code))
effects_coastal

for(i in 1:length(effects_coastal$code))
{
  temp_df <- fsn(effects_coastal[1:i,]$yi, effects_coastal[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_coastal[1:i,]$yi)+10)
  effects_coastal$failsaferatio[i] <- failsafe_one_run
  
}
effects_coastal
effects_coastal$failsaferatio
effects_coastal$order <- seq(1:16)

# Algae graph
gg_coastal <- ggplot(effects_coastal, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_coastal
gg_coastal <- gg_coastal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_coastal
gg_coastal <- gg_coastal + scale_y_continuous(trans = "reverse")
gg_coastal
gg_coastal <- gg_coastal + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_coastal
gg_coastal <- gg_coastal + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Coastal (N = 16)")
gg_coastal
gg_coastal <- gg_coastal + theme_bw()
gg_coastal <- gg_coastal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_coastal

################
effects_intertidal <- filter(ordered_by_year, ecosystemforheatmap == "intertidal")

effects_intertidal$fsr <- rep(NA, length(effects_intertidal$code))
effects_intertidal
for(i in 1:length(effects_intertidal$code))
{
  temp_df <- fsn(effects_intertidal[1:i,]$yi, effects_intertidal[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_intertidal[1:i,]$yi)+10)
  effects_intertidal$failsaferatio[i] <- failsafe_one_run
  
}
effects_intertidal
effects_intertidal$failsaferatio
effects_intertidal$order <- seq(1:14)

# Algae graph
gg_intertidal <- ggplot(effects_intertidal, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_intertidal
gg_intertidal <- gg_intertidal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_intertidal
gg_intertidal <- gg_intertidal + scale_y_continuous(trans = "reverse")
gg_intertidal
gg_intertidal <- gg_intertidal + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_intertidal
gg_intertidal <- gg_intertidal + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Intertidal (N = 14)")
gg_intertidal
gg_intertidal <- gg_intertidal + theme_bw()
gg_intertidal <- gg_intertidal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_intertidal

################
effects_urban <- filter(ordered_by_year, ecosystemforheatmap == "urban")

effects_urban$fsr <- rep(NA, length(effects_urban$code))
effects_urban
for(i in 1:length(effects_urban$code))
{
  temp_df <- fsn(effects_urban[1:i,]$yi, effects_urban[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_urban[1:i,]$yi)+10)
  effects_urban$failsaferatio[i] <- failsafe_one_run
  
}
effects_urban
effects_urban$failsaferatio
effects_urban$order <- seq(1:11)

# Algae graph
gg_urban <- ggplot(effects_urban, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_urban
gg_urban <- gg_urban + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_urban
gg_urban <- gg_urban + scale_y_continuous(trans = "reverse")
gg_urban
gg_urban <- gg_urban + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_urban
gg_urban <- gg_urban + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Urban (N = 11)")
gg_urban
gg_urban <- gg_urban + theme_bw()
gg_urban <- gg_urban + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_urban

################
effects_shrub <- filter(ordered_by_year, ecosystemforheatmap == "shrubland")

effects_shrub$fsr <- rep(NA, length(effects_shrub$code))
effects_shrub
for(i in 1:length(effects_shrub$code))
{
  temp_df <- fsn(effects_shrub[1:i,]$yi, effects_shrub[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_shrub[1:i,]$yi)+10)
  effects_shrub$failsaferatio[i] <- failsafe_one_run
  
}
effects_shrub
effects_shrub$failsaferatio
effects_shrub$order <- seq(1:10)

# Algae graph
gg_shrub <- ggplot(effects_shrub, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_shrub
gg_shrub <- gg_shrub + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_shrub
gg_shrub <- gg_shrub + scale_y_continuous(trans = "reverse")
gg_shrub
gg_shrub <- gg_shrub + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_shrub
gg_shrub <- gg_shrub + xlab("Failsafe ratio") +
  ylab("Publication order") +
  ggtitle("Shrubland (N = 10)")
gg_shrub
gg_shrub <- gg_shrub + theme_bw()
gg_shrub <- gg_shrub + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_shrub

grid.arrange(gg_forest,gg_lotic,gg_grass,gg_island,gg_estuarine,gg_lentic,gg_coastal,gg_intertidal,gg_urban,gg_shrub,ncol=5)
