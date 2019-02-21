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

# Algae slopes
effects_algae <- filter(ordered_by_year, invasivespeciestaxa == "algae and seaweed")
rma_algae <- rma(yi=effects_algae$yi,
                  vi=effects_algae$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_algae)

cma_algae <- viz_forest(x = rma_algae,
                         method = "REML",
                         xlab = "Response Ratio",
                         # variant = "thick",
                         type = "cumulative")
cma_algae

# Prep the for loop
effects_algae$cumulative_slope <- rep(NA, length(effects_algae$code))
cma_algae$data$order <- seq(1:22)
cma_algae$data

for (i in 1:22)
{
  temp_df <- lm(x ~ order, data = cma_algae$data[1:i,])
  effects_algae$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_algae
which(abs(effects_algae$cumulative_slope)< .005)
all_algae <- lm(x ~ order, data= cma_algae$data)
summary(all_algae)

# Stable plot Alge
effects_algae$order <- seq(1:22)
stable_algae <- ggplot(effects_algae, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_algae
stable_algae <- stable_algae + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_algae
stable_algae <- stable_algae + scale_y_continuous(trans = "reverse")
stable_algae
stable_algae <- stable_algae + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_algae <- stable_algae + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_algae <- stable_algae +
  xlab("Cumulative slopes") +
  ylab("Publication order") +
   ggtitle("Algae (N = 22)")
 stable_algae <- stable_algae + theme_bw()
 stable_algae <- stable_algae + theme(
   panel.grid.major.y = element_blank(),
   panel.grid.minor.y = element_blank(),
   legend.position = "none",
   plot.title = element_text(hjust = 0.5))
stable_algae

# Aquatic plants
effects_aquatic_plants <- filter(ordered_by_year, invasivespeciestaxa == "aquatic plant")
rma_aquatic <- rma(yi=effects_aquatic_plants$yi, 
                 vi=effects_aquatic_plants$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_aquatic_plants)

cma_aquatic <- viz_forest(x = rma_aquatic, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_aquatic

# Prep the for loop
effects_aquatic_plants$cumulative_slope <- rep(NA, length(effects_aquatic_plants$code))
cma_aquatic$data$order <- seq(1:8)
cma_aquatic$data

for (i in 1:8)
{
  temp_df <- lm(x ~ order, data = cma_aquatic$data[1:i,])
  effects_aquatic_plants$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_aquatic_plants
which(abs(effects_aquatic_plants$cumulative_slope)< .005)
all_aquatic <- lm(x ~ order, data= cma_aquatic$data)
summary(all_aquatic)

# Stable plot Alge
effects_aquatic_plants$order <- seq(1:8)
stable_aquatic <- ggplot(effects_aquatic_plants, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_aquatic
stable_aquatic <- stable_aquatic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_aquatic
stable_aquatic <- stable_aquatic + scale_y_continuous(trans = "reverse")
stable_aquatic
stable_aquatic <- stable_aquatic + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_aquatic <- stable_aquatic + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)
stable_aquatic <- stable_aquatic + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Aquatic plants (N = 8)")
stable_aquatic <- stable_aquatic + theme_bw()
stable_aquatic <- stable_aquatic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_aquatic

#################
effects_crust <- filter(ordered_by_year, invasivespeciestaxa == "crustacean")

rma_crust <- rma(yi=effects_crust$yi, 
                 vi=effects_crust$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_crust)

cma_crust <- viz_forest(x = rma_crust, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_crust

# Prep the for loop
effects_crust$cumulative_slope <- rep(NA, length(effects_crust$code))
cma_crust$data$order <- seq(1:23)
cma_crust$data

for (i in 1:23)
{
  temp_df <- lm(x ~ order, data = cma_crust$data[1:i,])
  effects_crust$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_crust
which(abs(effects_crust$cumulative_slope)< .005)
all_crust <- lm(x ~ order, data= cma_crust$data)
summary(all_crust)

# Stable plot Alge
effects_crust$order <- seq(1:23)
stable_crust <- ggplot(effects_crust, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_crust
stable_crust <- stable_crust + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_crust
stable_crust <- stable_crust + scale_y_continuous(trans = "reverse")
stable_crust
stable_crust <- stable_crust + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_crust <- stable_crust + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)
stable_crust <- stable_crust + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Crustacean (N = 23)")
stable_crust <- stable_crust + theme_bw()
stable_crust <- stable_crust + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_crust

############ FISH
effects_fish <- filter(ordered_by_year, invasivespeciestaxa == "fish")
rma_fish <- rma(yi=effects_fish$yi, 
                 vi=effects_fish$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_fish)

cma_fish <- viz_forest(x = rma_fish, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_fish

# Prep the for loop
effects_fish$cumulative_slope <- rep(NA, length(effects_fish$code))
cma_fish$data$order <- seq(1:19)
cma_fish$data

for (i in 1:19)
{
  temp_df <- lm(x ~ order, data = cma_fish$data[1:i,])
  effects_fish$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_fish
which(abs(effects_fish$cumulative_slope)< .005)
all_fish <- lm(x ~ order, data= cma_fish$data)
summary(all_fish)

# Stable plot Alge
effects_fish$order <- seq(1:19)
stable_fish <- ggplot(effects_fish, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_fish
stable_fish <- stable_fish + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_fish
stable_fish <- stable_fish + scale_y_continuous(trans = "reverse")
stable_fish
stable_fish <- stable_fish + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_fish <- stable_fish + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_fish <- stable_fish + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Fish (N = 19)")
stable_fish <- stable_fish + theme_bw()
stable_fish <- stable_fish + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_fish

############
effects_grasses <- filter(ordered_by_year, invasivespeciestaxa == "grasses")
effects_grasses
rma_grass <- rma(yi=effects_grasses$yi, 
                 vi=effects_grasses$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_grasses)

cma_grass <- viz_forest(x = rma_grass, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_grass

# Prep the for loop
effects_grasses$cumulative_slope <- rep(NA, length(effects_grasses$code))
effects_grasses
cma_grass
cma_grass$data$order <- seq(1:38)

for (i in 1:38)
{
  temp_df <- lm(x ~ order, data = cma_grass$data[1:i,])
  effects_grasses$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_grasses
which(abs(effects_grasses$cumulative_slope)< .005)
all_grasses <- lm(x ~ order, data= cma_grass$data)
summary(all_grasses)

# Stable plot Alge
effects_grasses$order <- seq(1:38)
stable_grass <- ggplot(effects_grasses, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_grass
stable_grass <- stable_grass + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_grass
stable_grass <- stable_grass + scale_y_continuous(trans = "reverse")
stable_grass
stable_grass <- stable_grass + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_grass <- stable_grass + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)

stable_grass <- stable_grass + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Grasses (N = 38)")
stable_grass <- stable_grass + theme_bw()
stable_grass <- stable_grass + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_grass

#############
effects_herb <- filter(ordered_by_year, invasivespeciestaxa == "herbaceous plant")
rma_herb <- rma(yi=effects_herb$yi, 
                 vi=effects_herb$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_herb)

cma_herb <- viz_forest(x = rma_herb, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_herb

# Prep the for loop
effects_herb$cumulative_slope <- rep(NA, length(effects_herb$code))
cma_herb$data$order <- seq(1:79)
cma_herb$data

for (i in 1:79)
{
  temp_df <- lm(x ~ order, data = cma_herb$data[1:i,])
  effects_herb$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_herb
which(abs(effects_herb$cumulative_slope)< .005)
all_herb <- lm(x ~ order, data= cma_herb$data)
summary(all_herb)

# Stable plot Alge
effects_herb$order <- seq(1:79)
stable_herb <- ggplot(effects_herb, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_herb <- stable_herb + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_herb
stable_herb <- stable_herb + scale_y_continuous(trans = "reverse")
stable_herb
stable_herb <- stable_herb + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_herb <- stable_herb + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)

stable_herb <- stable_herb + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Herbaceous plants (N = 79)")
stable_herb <- stable_herb + theme_bw()
stable_herb <- stable_herb + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_herb

###############
effects_insect <- filter(ordered_by_year, invasivespeciestaxa == "insect")
rma_insect <- rma(yi=effects_insect$yi, 
                 vi=effects_insect$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_insect)

cma_insect <- viz_forest(x = rma_insect, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_insect

# Prep the for loop
effects_insect$cumulative_slope <- rep(NA, length(effects_insect$code))
effects_insect
cma_insect$data$order <- seq(1:27)
cma_insect$data

for (i in 1:27)
{
  temp_df <- lm(x ~ order, data = cma_insect$data[1:i,])
  effects_insect$cumulative_slope[i] <- temp_df$coefficients[2]
}

effects_insect
which(abs(effects_insect$cumulative_slope)< .005)
all_insect <- lm(x ~ order, data= cma_insect$data)
summary(all_insect)

# Stable plot Alge
effects_insect$order <- seq(1:27)
stable_insect <- ggplot(effects_insect, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_insect
stable_insect <- stable_insect + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_insect
stable_insect <- stable_insect + scale_y_continuous(trans = "reverse")
stable_insect
stable_insect <- stable_insect + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_insect <- stable_insect + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)
stable_insect <- stable_insect + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Insects (N = 27)")
stable_insect <- stable_insect + theme_bw()
stable_insect <- stable_insect + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_insect

###########
effects_mammal <- filter(ordered_by_year, invasivespeciestaxa == "mammal")
rma_mammal <- rma(yi=effects_mammal$yi, 
                 vi=effects_mammal$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_mammal)

cma_mammal <- viz_forest(x = rma_mammal, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_mammal

# Prep the for loop
effects_mammal$cumulative_slope <- rep(NA, length(effects_mammal$code))
cma_mammal$data$order <- seq(1:16)
cma_mammal$data

for (i in 1:16)
{
  temp_df <- lm(x ~ order, data = cma_mammal$data[1:i,])
  effects_mammal$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_mammal
which(abs(effects_mammal$cumulative_slope)< .005)
all_mammal <- lm(x ~ order, data= cma_mammal$data)
summary(all_mammal)

# Stable plot Alge
effects_mammal$order <- seq(1:16)
stable_mammal <- ggplot(effects_mammal, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_mammal
stable_mammal <- stable_mammal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_mammal
stable_mammal <- stable_mammal + scale_y_continuous(trans = "reverse")
stable_mammal
stable_mammal <- stable_mammal + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_mammal <- stable_mammal + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)
stable_mammal <- stable_mammal + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Mammals (N = 16)")
stable_mammal <- stable_mammal + theme_bw()
stable_mammal <- stable_mammal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_mammal

###############
effects_molluscs <- filter(ordered_by_year, invasivespeciestaxa == "molluscs")
rma_molluscs <- rma(yi=effects_molluscs$yi, 
                 vi=effects_molluscs$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_molluscs)

cma_molluscs <- viz_forest(x = rma_molluscs, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_molluscs

# Prep the for loop
effects_molluscs$cumulative_slope <- rep(NA, length(effects_molluscs$code))
cma_molluscs$data$order <- seq(1:7)
cma_molluscs$data

for (i in 1:7)
{
  temp_df <- lm(x ~ order, data = cma_molluscs$data[1:i,])
  effects_molluscs$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_molluscs
which(abs(effects_molluscs$cumulative_slope)< .005)
all_molluscs <- lm(x ~ order, data= cma_molluscs$data)
summary(all_molluscs)

# Stable plot Alge
effects_molluscs$order <- seq(1:7)
stable_mollusks <- ggplot(effects_molluscs, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_mollusks
stable_mollusks <- stable_mollusks + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_mollusks
stable_mollusks <- stable_mollusks + scale_y_continuous(trans = "reverse")
stable_mollusks
stable_mollusks <- stable_mollusks + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_mollusks <- stable_mollusks + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)

stable_mollusks <- stable_mollusks + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Mollusks (N = 7)")
stable_mollusks <- stable_mollusks + theme_bw()
stable_mollusks <- stable_mollusks + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_mollusks

#############
effects_tree <- filter(ordered_by_year, invasivespeciestaxa == "tree")
rma_tree <- rma(yi=effects_tree$yi, 
                 vi=effects_tree$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_tree)

cma_tree <- viz_forest(x = rma_tree, 
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_tree

# Prep the for loop
effects_tree$cumulative_slope <- rep(NA, length(effects_tree$code))
cma_tree$data$order <- seq(1:88)
cma_tree$data

for (i in 1:88)
{
  temp_df <- lm(x ~ order, data = cma_tree$data[1:i,])
  effects_tree$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_tree
which(abs(effects_tree$cumulative_slope)< .005)
all_tree <- lm(x ~ order, data= cma_tree$data)
summary(all_tree)

# Stable plot Alge
effects_tree$order <- seq(1:88)
stable_tree <- ggplot(effects_tree, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_tree
stable_tree <- stable_tree + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_tree
stable_tree <- stable_tree + scale_y_continuous(trans = "reverse")
stable_tree
stable_tree <- stable_tree + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_tree <- stable_tree + geom_vline(xintercept = -0.005, colour = "red", size = .5, linetype = 2)
stable_tree <- stable_tree + 
  xlab("Cumulative slopes") +
  ylab("Publication order") +
  ggtitle("Trees (N = 88)")
stable_tree <- stable_tree + theme_bw()
stable_tree <- stable_tree + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_tree

grid.arrange(stable_tree, stable_herb, stable_grass, stable_insect,stable_crust,stable_algae, stable_fish,stable_mammal, stable_aquatic, stable_mollusks, ncol=5)
dev.off()
