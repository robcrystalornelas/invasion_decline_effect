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

##### Forests
effects_forest <- filter(ordered_by_year, ecosystemforheatmap == "forest")

rma_forest <- rma(yi=effects_forest$yi,
                 vi=effects_forest$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_forest)
rma_forest

rma_forest_five <- rma(yi=effects_forest[1:15,]$yi,
                  vi=effects_forest[1:15,]$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_forest[1:15,])
rma_forest_five

cma_forest <- viz_forest(x = rma_forest,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_forest

# Prep the for loop
effects_forest$cumulative_slope <- rep(NA, length(effects_forest$code))
cma_forest$data$order <- seq(1:75)
cma_forest$data

for (i in 1:75)
{
  temp_df <- lm(x ~ order, data = cma_forest$data[1:i,])
  effects_forest$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_forest
which(abs(effects_forest$cumulative_slope)< .005)
all_forest <- lm(x ~ order, data= cma_forest$data)
summary(all_forest)

# Stable plot Alge
effects_forest$order <- seq(1:75)
counted_forest <- plyr::count(effects_forest$publicationyear)

stable_forest <- ggplot(effects_forest, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_forest
stable_forest <- stable_forest + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_forest
stable_forest <- stable_forest + scale_y_continuous(trans = "reverse", labels = counted_forest$x, breaks = which(forest_labels != ""))
stable_forest
stable_forest <- stable_forest + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_forest <- stable_forest + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_forest <- stable_forest +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Forest (N = 75)")
stable_forest <- stable_forest + theme_bw()
stable_forest <- stable_forest + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_forest

############ LOTIC
effects_lotic <- filter(ordered_by_year, ecosystemforheatmap == "lotic")

rma_lotic <- rma(yi=effects_lotic$yi,
                 vi=effects_lotic$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_lotic)
rma_lotic
cma_lotic <- viz_forest(x = rma_lotic,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_lotic

# Prep the for loop
effects_lotic$cumulative_slope <- rep(NA, length(effects_lotic$code))
cma_lotic$data$order <- seq(1:46)
cma_lotic$data

for (i in 1:46)
{
  temp_df <- lm(x ~ order, data = cma_lotic$data[1:i,])
  effects_lotic$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_lotic
which(abs(effects_lotic$cumulative_slope)< .005)
all_lotic <- lm(x ~ order, data= cma_lotic$data)
summary(all_lotic)

# Stable plot Alge
effects_lotic$order <- seq(1:46)
counted_lotic <- plyr::count(effects_lotic$publicationyear)

stable_lotic <- ggplot(effects_lotic, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_lotic
stable_lotic <- stable_lotic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_lotic
stable_lotic <- stable_lotic + scale_y_continuous(trans = "reverse", labels = counted_lotic$x, breaks = which(lotic_labels != ""))
stable_lotic
stable_lotic <- stable_lotic + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_lotic <- stable_lotic + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_lotic <- stable_lotic +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Lotic (N = 46)")
stable_lotic <- stable_lotic + theme_bw()
stable_lotic <- stable_lotic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_lotic

############# Grassland
effects_grassland <- filter(ordered_by_year, ecosystemforheatmap == "grassland")

rma_grassland <- rma(yi=effects_grassland$yi,
                 vi=effects_grassland$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_grassland)

cma_grassland <- viz_forest(x = rma_grassland,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_grassland

# Prep the for loop
effects_grassland$cumulative_slope <- rep(NA, length(effects_grassland$code))
cma_grassland$data$order <- seq(1:43)
cma_grassland$data

for (i in 1:43)
{
  temp_df <- lm(x ~ order, data = cma_grassland$data[1:i,])
  effects_grassland$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_grassland
which(abs(effects_grassland$cumulative_slope)< .005)
all_grassland <- lm(x ~ order, data= cma_grassland$data)
summary(all_grassland)

# Stable plot Alge
effects_grassland$order <- seq(1:43)
counted_grassland <- plyr::count(effects_grassland$publicationyear)
stable_grassland <- ggplot(effects_grassland, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_grassland
stable_grassland <- stable_grassland + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_grassland
stable_grassland <- stable_grassland + scale_y_continuous(trans = "reverse", labels = counted_grassland$x, breaks = which(grassland_labels != ""))
stable_grassland
stable_grassland <- stable_grassland + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_grassland <- stable_grassland + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_grassland <- stable_grassland +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Grassland (N = 43)")
stable_grassland <- stable_grassland + theme_bw()
stable_grassland <- stable_grassland + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_grassland

########### ISLAND
effects_island <- filter(ordered_by_year, ecosystemforheatmap == "island")
rma_island <- rma(yi=effects_island$yi,
                 vi=effects_island$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_island)

cma_island <- viz_forest(x = rma_island,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_island

# Prep the for loop
effects_island$cumulative_slope <- rep(NA, length(effects_island$code))
cma_island$data$order <- seq(1:35)
cma_island$data

for (i in 1:35)
{
  temp_df <- lm(x ~ order, data = cma_island$data[1:i,])
  effects_island$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_island
which(abs(effects_island$cumulative_slope)< .005)
all_island <- lm(x ~ order, data= cma_island$data)
summary(all_island)

# Stable plot Alge
effects_island$order <- seq(1:35)
counted_island <- plyr::count(effects_island$publicationyear)
stable_island <- ggplot(effects_island, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_island
stable_island <- stable_island + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_island
stable_island <- stable_island + scale_y_continuous(trans = "reverse", labels = counted_island$x, breaks = which(island_labels != ""))
stable_island
stable_island <- stable_island + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_island <- stable_island + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_island <- stable_island +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Island (N = 35)")
stable_island <- stable_island + theme_bw()
stable_island <- stable_island + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_island

############# ESTUARY
effects_estuarine <- filter(ordered_by_year, ecosystemforheatmap == "estuarine")
rma_estuarine <- rma(yi=effects_estuarine$yi,
                 vi=effects_estuarine$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_estuarine)

cma_estuarine <- viz_forest(x = rma_estuarine,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_estuarine

# Prep the for loop
effects_estuarine$cumulative_slope <- rep(NA, length(effects_estuarine$code))
cma_estuarine$data$order <- seq(1:34)
cma_estuarine$data

for (i in 1:34)
{
  temp_df <- lm(x ~ order, data = cma_estuarine$data[1:i,])
  effects_estuarine$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_estuarine
which(abs(effects_estuarine$cumulative_slope)< .005)
all_estuarine <- lm(x ~ order, data= cma_estuarine$data)
summary(all_estuarine)

# Stable plot Alge
effects_estuarine$order <- seq(1:34)
counted_estuarine <- plyr::count(effects_estuarine$publicationyear)
stable_estuarine <- ggplot(effects_estuarine, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_estuarine
stable_estuarine <- stable_estuarine + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_estuarine
stable_estuarine <- stable_estuarine + scale_y_continuous(trans = "reverse", labels = counted_estuarine$x, breaks = which(estuarine_labels != ""))
stable_estuarine
stable_estuarine <- stable_estuarine + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_estuarine <- stable_estuarine + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_estuarine <- stable_estuarine +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Estuarine (N = 34)")
stable_estuarine <- stable_estuarine + theme_bw()
stable_estuarine <- stable_estuarine + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_estuarine

###########  LENTIC
effects_lentic <- filter(ordered_by_year, ecosystemforheatmap == "lentic")
rma_lentic <- rma(yi=effects_lentic$yi,
                 vi=effects_lentic$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_lentic)

cma_lentic <- viz_forest(x = rma_lentic,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_lentic

# Prep the for loop
effects_lentic$cumulative_slope <- rep(NA, length(effects_lentic$code))
cma_lentic$data$order <- seq(1:30)
cma_lentic$data

for (i in 1:30)
{
  temp_df <- lm(x ~ order, data = cma_lentic$data[1:i,])
  effects_lentic$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_lentic
which(abs(effects_lentic$cumulative_slope)< .005)
all_lentic <- lm(x ~ order, data= cma_lentic$data)
summary(all_lentic)

# Stable plot Alge
effects_lentic$order <- seq(1:30)
counted_lentic <- plyr::count(effects_lentic$publicationyear)
stable_lentic <- ggplot(effects_lentic, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_lentic
stable_lentic <- stable_lentic + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_lentic
stable_lentic <- stable_lentic + scale_y_continuous(trans = "reverse", labels = counted_lentic$x, breaks = which(lentic_labels != ""))
stable_lentic
stable_lentic <- stable_lentic + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_lentic <- stable_lentic + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_lentic <- stable_lentic +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Lentic (N = 30)")
stable_lentic <- stable_lentic + theme_bw()
stable_lentic <- stable_lentic + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_lentic

########### COASTAL
effects_coastal <- filter(ordered_by_year, ecosystemforheatmap == "coastal")
rma_coastal <- rma(yi=effects_coastal$yi,
                 vi=effects_coastal$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_coastal)
rma_coastal
cma_coastal <- viz_forest(x = rma_coastal,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_coastal

# Prep the for loop
effects_coastal$cumulative_slope <- rep(NA, length(effects_coastal$code))
cma_coastal$data$order <- seq(1:16)
cma_coastal$data

for (i in 1:16)
{
  temp_df <- lm(x ~ order, data = cma_coastal$data[1:i,])
  effects_coastal$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_coastal
which(abs(effects_coastal$cumulative_slope)< .005)
all_coastal <- lm(x ~ order, data= cma_coastal$data)
summary(all_coastal)

# Stable plot Alge
effects_coastal$order <- seq(1:16)
coastal_count <- plyr::count(effects_coastal$publicationyear)
stable_coastal <- ggplot(effects_coastal, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_coastal
stable_coastal <- stable_coastal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_coastal
stable_coastal <- stable_coastal + scale_y_continuous(trans = "reverse", labels = coastal_count$x, breaks = which(coastal_labels != ""))
stable_coastal
stable_coastal <- stable_coastal + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_coastal <- stable_coastal + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_coastal <- stable_coastal +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Coastal (N = 16)")
stable_coastal <- stable_coastal + theme_bw()
stable_coastal <- stable_coastal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_coastal


#############  INTERTIDAL
effects_intertidal <- filter(ordered_by_year, ecosystemforheatmap == "intertidal")
effects_intertidal
rma_intertidal <- rma(yi=effects_intertidal$yi,
                 vi=effects_intertidal$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_intertidal)

cma_intertidal <- viz_forest(x = rma_intertidal,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_intertidal

# Prep the for loop
effects_intertidal$cumulative_slope <- rep(NA, length(effects_intertidal$code))
cma_intertidal$data$order <- seq(1:14)
cma_intertidal$data

for (i in 1:14)
{
  temp_df <- lm(x ~ order, data = cma_intertidal$data[1:i,])
  effects_intertidal$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_intertidal
which(abs(effects_intertidal$cumulative_slope)< .005)
all_intertidal <- lm(x ~ order, data= cma_intertidal$data)
summary(all_intertidal)

# Stable plot Alge
effects_intertidal$order <- seq(1:14)
count_intertidal <- plyr::count(effects_intertidal$publicationyear)
stable_intertidal <- ggplot(effects_intertidal, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_intertidal
stable_intertidal <- stable_intertidal + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_intertidal
stable_intertidal <- stable_intertidal + scale_y_continuous(trans = "reverse", labels = count_intertidal$x, breaks = which(intertidal_labels != ""))
stable_intertidal
stable_intertidal <- stable_intertidal + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_intertidal <- stable_intertidal + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_intertidal <- stable_intertidal +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Intertidal (N = 14)")
stable_intertidal <- stable_intertidal + theme_bw()
stable_intertidal <- stable_intertidal + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_intertidal

############ URBAN
effects_urban <- filter(ordered_by_year, ecosystemforheatmap == "urban")
rma_urban <- rma(yi=effects_urban$yi,
                 vi=effects_urban$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_urban)

cma_urban <- viz_forest(x = rma_urban,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_urban

# Prep the for loop
effects_urban$cumulative_slope <- rep(NA, length(effects_urban$code))
cma_urban$data$order <- seq(1:11)
cma_urban$data

for (i in 1:11)
{
  temp_df <- lm(x ~ order, data = cma_urban$data[1:i,])
  effects_urban$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_urban
which(abs(effects_urban$cumulative_slope)< .005)
all_urban <- lm(x ~ order, data= cma_urban$data)
summary(all_urban)

# Stable plot Alge
effects_urban$order <- seq(1:11)
count_urban <- plyr::count(effects_urban$publicationyear)
stable_urban <- ggplot(effects_urban, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_urban
stable_urban <- stable_urban + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_urban
stable_urban <- stable_urban + scale_y_continuous(trans = "reverse", labels = count_urban$x, breaks = which(urban_labels != ""))
stable_urban
stable_urban <- stable_urban + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_urban <- stable_urban + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_urban <- stable_urban +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Urban (N = 11)")
stable_urban <- stable_urban + theme_bw()
stable_urban <- stable_urban + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_urban


############ Shrubland
effects_shrubland <- filter(ordered_by_year, ecosystemforheatmap == "shrubland")
effects_shrubland
rma_shrubland <- rma(yi=effects_shrubland$yi,
                 vi=effects_shrubland$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_shrubland)

cma_shrubland <- viz_forest(x = rma_shrubland,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative")
cma_shrubland

# Prep the for loop
effects_shrubland$cumulative_slope <- rep(NA, length(effects_shrubland$code))
cma_shrubland$data$order <- seq(1:10)
cma_shrubland$data

for (i in 1:10)
{
  temp_df <- lm(x ~ order, data = cma_shrubland$data[1:i,])
  effects_shrubland$cumulative_slope[i] <- temp_df$coefficients[2]
}
effects_shrubland
which(abs(effects_shrubland$cumulative_slope)< .005)
all_shrubland <- lm(x ~ order, data= cma_shrubland$data)
summary(all_shrubland)

# Stable plot Alge
effects_shrubland$order <- seq(1:10)
shrub_count <- plyr::count(effects_shrubland$publicationyear)
stable_shrubland <- ggplot(effects_shrubland, aes(x = cumulative_slope, y = order)) + geom_point(aes(colour = cumulative_slope < .005 & cumulative_slope > -.005))
stable_shrubland
stable_shrubland <- stable_shrubland + scale_colour_manual(values = setNames(c('red','black'),c(T, F)))
stable_shrubland
stable_shrubland <- stable_shrubland + scale_y_continuous(trans = "reverse", labels = shrub_count$x, breaks = which(shrubland_labels != ""))
stable_shrubland
stable_shrubland <- stable_shrubland + geom_vline(xintercept = 0.005, colour = "red", size = .5, linetype = 2)
stable_shrubland <- stable_shrubland + geom_vline(xintercept = -.005, colour = "red", size = .5, linetype = 2)
stable_shrubland <- stable_shrubland +
  xlab("Cumulative slopes") +
  ylab("publication year") +
  ggtitle("Shrubland (N = 10)")
stable_shrubland <- stable_shrubland + theme_bw()
stable_shrubland <- stable_shrubland + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5))
stable_shrubland

### Put them all together
grid.arrange(stable_forest, stable_lotic, stable_grassland, stable_island,stable_estuarine,stable_lentic,stable_coastal,stable_intertidal,stable_urban,stable_shrubland, ncol=5)
