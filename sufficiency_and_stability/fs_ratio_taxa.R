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
levels(ordered_by_year$invasivespeciestaxa)

# FSR algae
effects_algae <- filter(ordered_by_year, invasivespeciestaxa == "algae and seaweed")
effects_algae

effects_algae$fsr <- rep(NA, length(effects_algae$code))
effects_algae
for(i in 1:length(effects_algae$code))
{
  temp_df <- fsn(effects_algae[1:i,]$yi, effects_algae[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_algae[1:i,]$yi)+10)
  effects_algae$failsaferatio[i] <- failsafe_one_run
  
}
effects_algae
effects_algae$failsaferatio
effects_algae$order <- seq(1:22)

# Algae graph
gg_algae <- ggplot(effects_algae, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_algae
gg_algae <- gg_algae + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_algae
gg_algae <-  gg_algae + scale_y_continuous(trans = "reverse", labels = counted_all_algae$x, breaks = which(algae_labels != ""))
gg_algae
gg_algae <- gg_algae + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_algae
gg_algae <- gg_algae + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Algae (N = 22)")
gg_algae
gg_algae <- gg_algae + theme_bw()
gg_algae <- gg_algae + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_algae


# aquatic plants
effects_aquatic_plants <- filter(ordered_by_year, invasivespeciestaxa == "aquatic plant")
effects_aquatic_plants
fsn(yi, vi, data=effects_aquatic_plants)

effects_aquatic_plants$fsr <- rep(NA, length(effects_aquatic_plants$code))
effects_aquatic_plants
for(i in 1:length(effects_aquatic_plants$code))
{
  temp_df <- fsn(effects_aquatic_plants[1:i,]$yi, effects_aquatic_plants[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_aquatic_plants[1:i,]$yi)+10)
  effects_aquatic_plants$failsaferatio[i] <- failsafe_one_run
  
}
effects_aquatic_plants
effects_aquatic_plants$failsaferatio
effects_aquatic_plants$order <- seq(1:8)

# Aquatic graph
gg_aquatic <- ggplot(effects_aquatic_plants, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_aquatic
gg_aquatic <- gg_aquatic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_aquatic
gg_aquatic <- gg_aquatic + scale_y_continuous(trans = "reverse", labels = counted_aquatic$x, breaks = which(aquatic_labels != ""))
gg_aquatic
gg_aquatic <- gg_aquatic + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_aquatic
gg_aquatic <- gg_aquatic + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Aquatic plants (N = 8)")
gg_aquatic
gg_aquatic <- gg_aquatic + theme_bw()
gg_aquatic <- gg_aquatic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_aquatic

# Crustacean Failsafe
effects_crust <- filter(ordered_by_year, invasivespeciestaxa == "crustacean")
effects_crust

effects_crust$fsr <- rep(NA, length(effects_crust$code))
effects_crust
for(i in 1:length(effects_crust$code))
{
  temp_df <- fsn(effects_crust[1:i,]$yi, effects_crust[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_crust[1:i,]$yi)+10)
  effects_crust$failsaferatio[i] <- failsafe_one_run
  
}
effects_crust
effects_crust$failsaferatio
effects_crust$order <- seq(1:23)

# Crust graph
gg_crust <- ggplot(effects_crust, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_crust
gg_crust <- gg_crust + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_crust
gg_crust <-  gg_crust + scale_y_continuous(trans = "reverse", labels = crust_counted$x, breaks = which(crust_labels != ""))
gg_crust
gg_crust <- gg_crust + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_crust
gg_crust <- gg_crust + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Crustacean (N = 23)")
gg_crust
gg_crust <- gg_crust + theme_bw()
gg_crust <- gg_crust + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_crust


# FSR fish
effects_fish <- filter(ordered_by_year, invasivespeciestaxa == "fish")
effects_fish

effects_fish$fsr <- rep(NA, length(effects_fish$code))
effects_fish
for(i in 1:length(effects_fish$code))
{
  temp_df <- fsn(effects_fish[1:i,]$yi, effects_fish[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_fish[1:i,]$yi)+10)
  effects_fish$failsaferatio[i] <- failsafe_one_run
  
}
effects_fish
effects_fish$failsaferatio
effects_fish$order <- seq(1:19)

# Fish graph
gg_fish <- ggplot(effects_fish, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_fish
gg_fish <- gg_fish + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_fish
gg_fish <-  gg_fish + scale_y_continuous(trans = "reverse", labels = counted_fish$x, breaks = which(fish_labels != ""))
gg_fish
gg_fish <- gg_fish + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_fish
gg_fish <- gg_fish + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Fish (N = 19)")
gg_fish
gg_fish <- gg_fish + theme_bw()
gg_fish <- gg_fish + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_fish

# FSN grasses
effects_grass <- filter(ordered_by_year, invasivespeciestaxa == "grasses")

effects_grass$fsr <- rep(NA, length(effects_grass$code))
effects_grass

for(i in 1:length(effects_grass$code))
{
  temp_df <- fsn(effects_grass[1:i,]$yi, effects_grass[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_grass[1:i,]$yi)+10)
  effects_grass$failsaferatio[i] <- failsafe_one_run
  
}
effects_grass
effects_grass$failsaferatio
effects_grass$order <- seq(1:38)

# Grass graph
gg_grass <- ggplot(effects_grass, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_grass
gg_grass <- gg_grass + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_grass
gg_grass <-  gg_grass + scale_y_continuous(trans = "reverse", labels = counted_grass$x, breaks = which(grass_labels != ""))
gg_grass
gg_grass <- gg_grass + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_grass
gg_grass <- gg_grass + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Grasses (N = 38)")
gg_grass
gg_grass <- gg_grass + theme_bw()
gg_grass <- gg_grass + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_grass


# FSR herbaceous plants
effects_herb <- filter(ordered_by_year, invasivespeciestaxa == "herbaceous plant")

effects_herb$fsr <- rep(NA, length(effects_herb$code))
effects_herb
for(i in 1:length(effects_herb$code))
{
  temp_df <- fsn(effects_herb[1:i,]$yi, effects_herb[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_herb[1:i,]$yi)+10)
  effects_herb$failsaferatio[i] <- failsafe_one_run
  
}
effects_herb
effects_herb$failsaferatio
effects_herb$order <- seq(1:79)

# Herb graph
gg_herb <- ggplot(effects_herb, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_herb
gg_herb <- gg_herb + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_herb
gg_herb <-  gg_herb + scale_y_continuous(trans = "reverse", labels = counted_herb$x, breaks = which(herb_labels != ""))
gg_herb
gg_herb <- gg_herb + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_herb
gg_herb <- gg_herb + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Heabaceous plants (N = 79)")
gg_herb
gg_herb <- gg_herb + theme_bw()
gg_herb <- gg_herb + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_herb

# FSN insect
effects_insect <- filter(ordered_by_year, invasivespeciestaxa == "insect")

effects_insect$fsr <- rep(NA, length(effects_insect$code))
effects_insect
for(i in 1:length(effects_insect$code))
{
  temp_df <- fsn(effects_insect[1:i,]$yi, effects_insect[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_insect[1:i,]$yi)+10)
  effects_insect$failsaferatio[i] <- failsafe_one_run
  
}
effects_insect
effects_insect$failsaferatio
effects_insect$order <- seq(1:27)

# insect graph
gg_insect <- ggplot(effects_insect, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_insect
gg_insect <- gg_insect + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_insect
gg_insect <-  gg_insect + scale_y_continuous(trans = "reverse", labels = counted_instect$x, breaks = which(insect_labels != ""))
gg_insect
gg_insect <- gg_insect + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_insect
gg_insect <- gg_insect + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Insects (N = 27)")
gg_insect
gg_insect <- gg_insect + theme_bw()
gg_insect <- gg_insect + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_insect

# FSN mammals
effects_mammal <- filter(ordered_by_year, invasivespeciestaxa == "mammal")

effects_mammal$fsr <- rep(NA, length(effects_mammal$code))
effects_mammal
for(i in 1:length(effects_mammal$code))
{
  temp_df <- fsn(effects_mammal[1:i,]$yi, effects_mammal[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_mammal[1:i,]$yi)+10)
  effects_mammal$failsaferatio[i] <- failsafe_one_run
  
}
effects_mammal
effects_mammal$failsaferatio
effects_mammal$order <- seq(1:16)

# mammal graph
gg_mammal <- ggplot(effects_mammal, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_mammal
gg_mammal <- gg_mammal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_mammal
gg_mammal <-  gg_mammal + scale_y_continuous(trans = "reverse", labels = counted_mammals$x, breaks = which(mammal_labels != ""))
gg_mammal
gg_mammal <- gg_mammal + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_mammal
gg_mammal <- gg_mammal + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Mammals (N = 16)")
gg_mammal
gg_mammal <- gg_mammal + theme_bw()
gg_mammal <- gg_mammal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_mammal

# FSN molluscs
effects_molluscs <- filter(ordered_by_year, invasivespeciestaxa == "molluscs")

effects_molluscs$fsr <- rep(NA, length(effects_molluscs$code))
effects_molluscs
for(i in 1:length(effects_molluscs$code))
{
  temp_df <- fsn(effects_molluscs[1:i,]$yi, effects_molluscs[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_molluscs[1:i,]$yi)+10)
  effects_molluscs$failsaferatio[i] <- failsafe_one_run
  
}
effects_molluscs
effects_molluscs$failsaferatio
effects_molluscs$order <- seq(1:7)

# moll graph
gg_moll <- ggplot(effects_molluscs, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_moll
gg_moll <- gg_moll + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_moll
gg_moll <-  gg_moll + scale_y_continuous(trans = "reverse", labels = counted_mollusks$x, breaks = which(moll_labels != ""))
gg_moll
gg_moll <- gg_moll + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_moll
gg_moll <- gg_moll + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Mollusks (N = 7)")
gg_moll
gg_moll <- gg_moll + theme_bw()
gg_moll <- gg_moll + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_moll

#FSN Trees
effects_tree <- filter(ordered_by_year, invasivespeciestaxa == "tree")

effects_tree$fsr <- rep(NA, length(effects_tree$code))
effects_tree
for(i in 1:length(effects_tree$code))
{
  temp_df <- fsn(effects_tree[1:i,]$yi, effects_tree[1:i,]$vi, type = "Rosenthal")
  temp_df$fsnum
  failsafe_one_run <- (temp_df$fsnum)/(5*length(effects_tree[1:i,]$yi)+10)
  effects_tree$failsaferatio[i] <- failsafe_one_run
  
}
effects_tree
effects_tree$failsaferatio
effects_tree$order <- seq(1:88)

# Tree graph
gg_tree <- ggplot(effects_tree, aes(x = failsaferatio, y = order)) + geom_point(aes(colour = failsaferatio > 1))
gg_tree
gg_tree <- gg_tree + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
gg_tree
gg_tree <-  gg_tree + scale_y_continuous(trans = "reverse", labels = counted_tree$x, breaks = which(tree_labels != ""))
gg_tree
gg_tree <- gg_tree + geom_vline(xintercept = 1, colour = "red", size = .5, linetype = 2)
gg_tree
gg_tree <- gg_tree + xlab("Failsafe ratio") +
  ylab("Publication year") +
  ggtitle("Trees (N = 88)")
gg_tree
gg_tree <- gg_tree + theme_bw()
gg_tree <- gg_tree + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
gg_tree

### Make full plot
grid.arrange(gg_tree, gg_herb, gg_grass, gg_insect, gg_crust, gg_algae, gg_fish, gg_mammal, gg_aquatic, gg_moll, ncol=5)

dev.off()
