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

# Forest
effects_forest <- filter(ordered_by_year, ecosystemforheatmap == "forest")
effects_forest

rma_forest <- rma(yi=effects_forest$yi, 
                 vi=effects_forest$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_forest)
rma_forest
exp(-.2078)
1-.8123
1-(exp(-.2078)*((sigma(rma_forest)^2)/2))

effects_forest[1:15,]

rma_forest_five <-rma(yi=effects_forest[1:15,]$yi, 
                      vi=effects_forest[1:15,]$vi,
                      method = "REML",
                      test = "knha",
                      data=effects_forest[1:15,]) 
rma_forest_five
1-(exp(-.5811)*((sigma(rma_forest_five)^2)/2))
1-exp(-.5811)
plyr::count(effects_forest$publicationyear)
forest_labels <- c(1999,
                   2001,
                 2003,
                 strrep("", 1:4),
                 2004,
                 strrep("", 1:4),
                 2005,
                 strrep("",1),
                 2006,
                 2007,
                 strrep("", 1:4),
                 2008,
                 strrep("", 1:4),
                 2009,
                 strrep("", 1),
                 2010,
                 strrep("",1:4),
                 2011,
                 strrep("",1:4),
                 2012,
                 strrep("",1:8),
                 2013,
                 strrep("",1:5),
                 2014,
                 strrep("",1:13),
                 2015,
                 strrep("",1:3),
                 2016,
                 strrep("",1:4))

forest_label_legible <- c(1999,
                          strrep("", 1),
                          2003,
                          strrep("", 1:4),
                          2004,
                          strrep("", 1:4),
                          2005,
                          strrep("",1:2),
                          2007,
                          strrep("", 1:4),
                          2008,
                          strrep("", 1:4),
                          2009,
                          strrep("", 1),
                          2010,
                          strrep("",1:4),
                          2011,
                          strrep("",1:4),
                          2012,
                          strrep("",1:8),
                          2013,
                          strrep("",1:5),
                          2014,
                          strrep("",1:13),
                          2015,
                          strrep("",1:3),
                          2016,
                          strrep("",1:4))

cma_forest <- viz_forest(x = rma_forest, 
                        #study_labels = effects_forest[, "publicationyear"], 
                        study_labels = forest_label_legible,
                        method = "REML",
                        xlab = "Response Ratio",
                        # variant = "thick",
                        type = "cumulative") +
                        ggtitle("Forest (N = 75)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_forest

cma_forest$data

# CMA for lotic
effects_lotic <- filter(ordered_by_year, ecosystemforheatmap == "lotic")
effects_lotic
fsn(yi,vi,data = effects_lotic)

rma_lotic <- rma(yi=effects_lotic$yi, 
                  vi=effects_lotic$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_lotic)
rma_lotic
exp(-.2585)
1-.772209
1-(exp(-.2585)*((sigma(rma_lotic)^2)/2))

rma_lotic_five <- rma(
  yi=effects_lotic[1:5,]$yi, 
  vi=effects_lotic[1:5,]$vi,
  method = "REML",
  test = "knha",
  data=effects_lotic[1:5,])
rma_lotic_five

plyr::count(effects_lotic$publicationyear)
lotic_labels <- c(1999,
                   2000,
                   strrep("",1),
                   2001,
                   2002,
                   strrep("",1),
                   2003,
                   strrep("", 1:2),
                   2004,
                   strrep("", 1),
                   2006,
                   strrep("", 1:2),
                   2007,
                   strrep("", 1),
                   2008,
                   strrep("", 1),
                   2010,
                   strrep("",1:2),
                   2012,
                   strrep("",1:4),
                   2013,
                   strrep("",1:4),
                   2014,
                   strrep("",1),
                   2015,
                   strrep("",1:2),
                   2016,
                   strrep("",1:9))

lotic_labels_legible <- c(1999,
                  strrep("",1:3),
                  2002,
                  strrep("",1),
                  2003,
                  strrep("", 1:2),
                  2004,
                  strrep("", 1),
                  2006,
                  strrep("", 1:2),
                  2007,
                  strrep("", 1),
                  2008,
                  strrep("", 1),
                  2010,
                  strrep("",1:2),
                  2012,
                  strrep("",1:4),
                  2013,
                  strrep("",1:4),
                  2014,
                  strrep("",1),
                  2015,
                  strrep("",1:2),
                  2016,
                  strrep("",1:9))

cma_lotic <- viz_forest(x = rma_lotic, 
                         #study_labels = effects_lotic[, "publicationyear"], 
                        study_labels = lotic_labels_legible,
                        method = "REML",
                         xlab = "Response Ratio",
                         type = "cumulative") +
                          ggtitle("Lotic (N = 46)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
axis.title = element_text(size = 14, colour = "black"),
axis.text = element_text(size = 14, colour = "black"))
cma_lotic
cma_lotic$data

# CMA grasses
effects_grassland <- filter(ordered_by_year, ecosystemforheatmap == "grassland")
effects_grassland
fsn(yi,vi,data = effects_grassland)

rma_grassland <- rma(yi=effects_grassland$yi, 
                  vi=effects_grassland$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_grassland)
rma_grassland

plyr::count(effects_grassland$publicationyear)
grassland_labels <- c(1999,
                   2002,
                   2004,
                   strrep("", 1),
                   2005,
                   strrep("", 1:3),
                   2006,
                   2007,
                   2008,
                   strrep("", 1:4),
                   2009,
                   strrep("", 1:5),
                   2010,
                   strrep("", 1:2),
                   2011,
                   strrep("",1:3),
                   2012,
                   strrep("",1),
                   2013,
                   strrep("",1:2),
                   2014,
                   strrep("",1:2),
                   2015,
                   strrep("",1:3),
                   2016,
                   strrep("",1:2))
grassland_labels_legible <- c(1999,
                              strrep("", 1),
                      2004,
                      strrep("", 1),
                      2005,
                      strrep("", 1:3),
                      2006,
                      strrep("", 1),
                      2008,
                      strrep("", 1:4),
                      2009,
                      strrep("", 1:5),
                      2010,
                      strrep("", 1:2),
                      2011,
                      strrep("",1:3),
                      2012,
                      strrep("",1),
                      2013,
                      strrep("",1:2),
                      2014,
                      strrep("",1:2),
                      2015,
                      strrep("",1:3),
                      2016,
                      strrep("",1:2))

cma_grassland <- viz_forest(x = rma_grassland, 
                         #study_labels = effects_grassland[, "publicationyear"], 
                         study_labels = grassland_labels_legible,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Grassland (N = 43)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_grassland

# CMA island
effects_island <- filter(ordered_by_year, ecosystemforheatmap == "island")
effects_island
fsn(yi,vi,data = effects_island,type = "Rosenberg")

rma_island <- rma(yi=effects_island$yi, 
                  vi=effects_island$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_island)
rma_island

plyr::count(effects_island$publicationyear)
island_labels <- c(1999,
                  2001,
                  strrep("",1:3),
                  2003,
                  2004,
                  strrep("", 1:2),
                  2006,
                  strrep("", 1),
                  2007,
                  strrep("", 1:4),
                  2010,
                  strrep("",1:2),
                  2011,
                  strrep("",1),
                  2014,
                  2015,
                  strrep("",1:4),
                  2016,
                  strrep("",1:7))

cma_island <- viz_forest(x = rma_island, 
                         #study_labels = effects_island[, "publicationyear"], 
                         study_labels = island_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Island (N = 35)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_island

# CMA estuarine
effects_estuarine <- filter(ordered_by_year, ecosystemforheatmap == "estuarine")
effects_estuarine
fsn(yi,vi,data = effects_estuarine)

rma_estuarine <- rma(yi=effects_estuarine$yi, 
                  vi=effects_estuarine$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_estuarine)
rma_estuarine
plyr::count(effects_estuarine$publicationyear)
estuarine_labels <- c(
                   2001,
                   2004,
                   2005,
                   2006,
                   strrep("",1:2),
                   2007,
                   2008,
                   2009,
                   strrep("", 1:7),
                   2010,
                   strrep("",1),
                   2011,
                   strrep("",1),
                   2012,
                   strrep("",1),
                   2013,
                   2014,
                   strrep("",1:6),
                   2015,
                   strrep("",1),
                   2016,
                   strrep("",1:2))

cma_estuarine <- viz_forest(x = rma_estuarine, 
                         #study_labels = effects_estuarine[, "publicationyear"], 
                         study_labels = estuarine_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Estuarine (N = 35)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_estuarine
cma_estuarine$data

effects_estuarine
rma_estuary_five <- rma(
  yi=effects_estuarine[1:5,]$yi, 
  vi=effects_estuarine[1:5,]$vi,
  method = "REML",
  test = "knha",
  data=effects_estuarine[1:5,])
rma_estuary_five

# CMA lentic
effects_lentic <- filter(ordered_by_year, ecosystemforheatmap == "lentic")
effects_lentic
fsn(yi,vi,data = effects_lentic)

rma_lentic <- rma(yi=effects_lentic$yi, 
                  vi=effects_lentic$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_lentic)
rma_lentic

plyr::count(effects_lentic$publicationyear)
lentic_labels <- c(1999,
                   2005,
                   2006,
                   strrep("",1),
                   2008,
                   strrep("", 1),
                   2009,
                   2010,
                   2011,
                   strrep("",1:2),
                   2012,
                   strrep("",1:5),
                   2014,
                   strrep("",1:3),
                   2015,
                   strrep("",1),
                   2016,
                   strrep("",1:6))

cma_lentic <- viz_forest(x = rma_lentic,
                         #study_labels = effects_lentic[, "publicationyear"], 
                         study_labels = lentic_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Lentic (N = 30)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_lentic

# CMA coastal
effects_coastal <- filter(ordered_by_year, ecosystemforheatmap == "coastal")
effects_coastal
fsn(yi,vi,data = effects_coastal)

rma_coastal <- rma(yi=effects_coastal$yi, 
                  vi=effects_coastal$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_coastal)
rma_coastal
1-exp(-0.3914)*((sigma(rma_coastal)^2)/2)

plyr::count(effects_coastal$publicationyear)
coastal_labels <- c(
                   2003,
                   2005,
                   2007,
                   2008,
                   strrep("", 1:3),
                   2009,
                   2011,
                   2013,
                   strrep("",1:3),
                   2014,
                   2015,
                   strrep("",1))

cma_coastal <- viz_forest(x = rma_coastal, 
                         #study_labels = effects_coastal[, "publicationyear"], 
                         study_labels = coastal_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Coastal (N = 16)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_coastal

# CMA rocky interidal
effects_intertidal <- filter(ordered_by_year, ecosystemforheatmap == "intertidal")
effects_intertidal
fsn(yi,vi,data = effects_intertidal)

rma_intertidal <- rma(yi=effects_intertidal$yi, 
                  vi=effects_intertidal$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_intertidal)
rma_intertidal

plyr::count(effects_intertidal$publicationyear)
intertidal_labels <- c(
                   2004,
                   2005,
                   2009,
                   2012,
                   strrep("",1),
                   2013,
                   2014,
                   strrep("",1:3),
                   2016,
                   strrep("",1:3))

cma_intertidal <- viz_forest(x = rma_intertidal, 
                         #study_labels = effects_intertidal[, "publicationyear"], 
                         study_labels = intertidal_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Intertidal (N = 14)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_intertidal

# CMA rocky interidal
effects_rocky_intertidal <- filter(ordered_by_year, ecosystemforheatmap == "rocky intertidal")
effects_rocky_intertidal
fsn(yi,vi,data = effects_rocky_intertidal)

rma_rocky_intertidal <- rma(yi=effects_rocky_intertidal$yi, 
                      vi=effects_rocky_intertidal$vi,
                      method = "REML",
                      test = "knha",
                      data=effects_rocky_intertidal)
rma_rocky_intertidal

plyr::count(effects_rocky_intertidal$publicationyear)
rocky_intertidal_labels <- c(
  2004,
  2005,
  2009,
  2012,
  2013,
  2014,
  strrep("",1:2),
  2016,
  strrep("",1))
rocky_intertidal_labels
cma_rocky_intertidal <- viz_forest(x = rma_rocky_intertidal, 
                             #study_labels = effects_intertidal[, "publicationyear"], 
                             study_labels = rocky_intertidal_labels,
                             method = "REML",
                             xlab = "Response Ratio",
                             #variant = "thick",
                             type = "cumulative") +
  ggtitle("Rocky intertidal (N = 10)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_rocky_intertidal

# CMA urban
effects_urban <- filter(ordered_by_year, ecosystemforheatmap == "urban")
effects_urban
fsn(yi,vi,data = effects_urban, type = "Rosenberg")

rma_urban <- rma(yi=effects_urban$yi, 
                  vi=effects_urban$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_urban)
rma_urban

plyr::count(effects_urban$publicationyear)
urban_labels <- c(
                   2006,
                   2007,
                   strrep("", 1:5),
                   2010,
                   strrep("",1),
                   2011,
                   2016)

cma_urban <- viz_forest(x = rma_urban, 
                         #study_labels = effects_urban[, "publicationyear"], 
                        study_labels = urban_labels, 
                        method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Urban (N = 11)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_urban

# CMA shrubland
effects_shrubland <- filter(ordered_by_year, ecosystemforheatmap == "shrubland")
effects_shrubland
fsn(yi,vi,data = effects_shrubland,type = "Rosenberg")

rma_shrubland <- rma(yi=effects_shrubland$yi, 
                  vi=effects_shrubland$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_shrubland)
rma_shrubland

plyr::count(effects_shrubland$publicationyear)
shrubland_labels <- c(2002,
                   strrep("",1),
                   2004,
                   strrep("", 1),
                   2009,
                   strrep("", 1:2),
                   2013,
                   2014,
                   strrep("",1))

cma_shrubland <- viz_forest(x = rma_shrubland, 
                         #study_labels = effects_shrubland[, "publicationyear"], 
                         study_labels = shrubland_labels,
                         method = "REML",
                         xlab = "Response Ratio",
                         #variant = "thick",
                         type = "cumulative") +
  ggtitle("Shrubland (N = 10)") +
  ylab("publication year") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_shrubland
cma_shrubland$data
# Combine all CMAs with more than 10 studies
grid.arrange(cma_forest,cma_lotic,cma_grassland,cma_island,cma_estuarine,cma_lentic,cma_coastal,cma_urban,cma_rocky_intertidal,cma_shrubland,ncol=5)

dev.off()

