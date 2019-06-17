## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(cowplot)

# Calculate effect size
effect_sizes_richness_imputed <-
  escalc(
    "ROM",
    # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
    m1i = raw_data_imputed$mean_invaded,
    n1i = raw_data_imputed$sample_size_invaded,
    # Then, follow with all of the columns needed to compute SMD
    sd1i = raw_data_imputed$SD_invaded,
    m2i = raw_data_imputed$mean_control,
    n2i = raw_data_imputed$sample_size_control,
    sd2i = raw_data_imputed$SD_control,
    data = raw_data_imputed
  )

ordered_by_year <-
  arrange(effect_sizes_richness_imputed, publicationyear)
head(ordered_by_year)
levels(ordered_by_year$invasivespeciestaxa)

# Only do CMAs on taxa with more than 10 cases ####
plyr::count(ordered_by_year$invasivespeciestaxa)
# Algae and seaweed
effects_algae <-
  filter(ordered_by_year, invasivespeciestaxa == "algae and seaweed")
effects_algae

# Get y-axis labels
effects_algae
plyr::count(effects_algae$publicationyear)

algae_labels <- c(2004,
                  2005,
                  2006,
                  strrep("", 1), 
                  2009,
                  strrep("", 1:3),
                  2010,
                  2011,
                  strrep("",1),
                  2012,
                  strrep("",1:2),
                  2013,
                  2014,
                  strrep("",1:3),
                  2016,
                  strrep("",1:2))
                
rma_algae <- rma(yi=effects_algae$yi, 
                 vi=effects_algae$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_algae)
rma_algae

rma_algae_five <- rma(
  yi=effects_algae[1:12,]$yi, 
                 vi=effects_algae[1:12,]$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_algae[1:12,])
rma_algae

fsn(yi, vi, data=effects_algae)
counted_all_algae <-plyr::count(effects_algae)
counted_all_algae
cma_algae <- viz_forest(
  x = rma_algae,
  #study_labels = effects_algae[, "publicationyear"],
  study_labels = algae_labels,
  method = "REML",
  xlab = "Response Ratio",
  summary_col = "Oranges",
  type = "cumulative"
) +
  ggtitle("Algae (N = 22)") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      colour = "black"
    ),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black")
  )
cma_algae
  
# aquatic plants
effects_aquatic_plants <- filter(ordered_by_year, invasivespeciestaxa == "aquatic plant")
effects_aquatic_plants
fsn(yi, vi, data=effects_aquatic_plants)

rma_aquatic_plants <- rma(yi=effects_aquatic_plants$yi, 
                 vi=effects_aquatic_plants$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_aquatic_plants)
rma_aquatic_plants

aquatic_labels<- c("1999","2010","2011","","","2012","2014","")
cma_aquatic_plants <- viz_forest(x = rma_aquatic_plants, 
                        #study_labels = effects_aquatic_plants[, "publicationyear"], 
                        study_labels = aquatic_labels,
                        method = "REML",
                        xlab = "Response Ratio",
                        summary_col = "Oranges",
                        type = "cumulative") +
  ggtitle("Aquatic plants (N = 8)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_aquatic_plants

# crustacean
effects_crust <- filter(ordered_by_year, invasivespeciestaxa == "crustacean")
effects_crust
fsn(yi, vi, data=effects_crust)

rma_crust <- rma(yi=effects_crust$yi, 
                 vi=effects_crust$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_crust)
rma_crust

exp(-0.2247)
1-0.7987558

# study labels
plyr::count(effects_crust$publicationyear)
crust_labels <- c(2003,
                  strrep("", 1),
                  2004,
                  2005,
                  2006,
                  strrep("", 1:2), 
                  2008,
                  strrep("", 1),
                  2012,
                  2013,
                  2014,
                  2015,
                  strrep("",1),
                  2016,
                  strrep("",1:8))

cma_crust <- viz_forest(x = rma_crust, 
                        #study_labels = effects_crust[, "publicationyear"], 
                        study_labels = crust_labels,
                        xlab = "Response Ratio",
                        summary_col = "Oranges",
                        method = "REML",
                        type = "cumulative") +
                        ggtitle("Crustacean (N = 23)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_crust


mean(cma_crust$data$x[1:5])
mean(cma_crust$data$x_min[1:5])
mean(cma_crust$data$x_max[1:5])

# fish
effects_fish <- filter(ordered_by_year, invasivespeciestaxa == "fish")
effects_fish
fsn(yi, vi, data=effects_fish)

rma_fish <- rma(yi=effects_fish$yi, 
                 vi=effects_fish$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_fish)
rma_fish

# make study labels
plyr::count(effects_fish$publicationyear)
fish_labels <- c(1999,
                  2000,
                  2001,
                  2008,
                  strrep("", 1), 
                  2010,
                  2012,
                 strrep("",1:6),
                  2013,
                  2014,
                  2015,
                  strrep("",1),
                  2016,
                  strrep("",1))

cma_fish <- viz_forest(x = rma_fish, 
                        #study_labels = effects_fish[, "publicationyear"], 
                       study_labels = fish_labels,
                        xlab = "Response Ratio",
                       summary_col = "Oranges",
                        method = "REML",
                        type = "cumulative") +
                        ggtitle("Fish (N = 19)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_fish

# grasses
effects_grass <- filter(ordered_by_year, invasivespeciestaxa == "grasses")
fsn(yi, vi, data=effects_grass)

rma_grass <- rma(yi=effects_grass$yi, 
                vi=effects_grass$vi,
                method = "REML",
                test = "knha",
                data=effects_grass)
rma_grass
exp( -0.3353)
1-0.7151235
plyr::count(effects_grass$publicationyear)
grass_labels <- c(1999,
                 2002,
                 strrep("", 1), 
                 2003,
                 2005,
                 strrep("", 1:5), 
                 2006,
                 2007,
                 2008,
                 2009,
                 strrep("",1:3),
                 2010,
                 strrep("",1:3),
                 2011,
                 strrep("",1),
                 2012,
                 strrep("",1:3),
                 2013,
                 strrep("",1),
                 2014,
                 2015,
                 strrep("",1:4),
                 2016,
                 strrep("",1:2))

grasses_legible <- c(1999,
                     strrep("", 1:2), 
                     2003,
                     strrep("", 1),
                     2005,
                     strrep("", 1:4), 
                     2006,
                     strrep("", 1),
                     2008,
                     strrep("", 1),
                     strrep("",1:3),
                     2010,
                     strrep("",1:3),
                     2011,
                     strrep("",1),
                     2012,
                     strrep("",1:3),
                     2013,
                     strrep("",1:2),
                     2015,
                     strrep("",1:4),
                     2016,
                     strrep("",1:2))

cma_grass <- viz_forest(x = rma_grass, 
                       #study_labels = effects_grass[, "publicationyear"], 
                       study_labels = grasses_legible,
                       xlab = "Response Ratio",
                       summary_col = "Oranges",
                       method = "REML",
                       type = "cumulative") +
                      ggtitle("Grasses (N = 38)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_grass

# herbaceous plant
effects_herb <- filter(ordered_by_year, invasivespeciestaxa == "herbaceous plant")
fsn(yi, vi, data=effects_herb)
rma_herb <- rma(yi=effects_herb$yi, 
                 vi=effects_herb$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_herb)
rma_herb

plyr::count(effects_herb$publicationyear)
herb_labels <- c(2001,
                  strrep("", 1:3), 
                  2003,
                  strrep("", 1),
                  2004,
                  strrep("", 1:7),
                  2006,
                  strrep("", 1), 
                  2007,
                  strrep("", 1:9),
                  2008,
                  strrep("", 1:2),
                  2009,
                  strrep("", 1:16),
                  2010,
                  strrep("",1:2),
                  2011,
                  strrep("",1:4),
                  2012,
                  strrep("",1:2),
                  2013,
                  strrep("",1:6),
                  2014,
                  strrep("",1:4),
                  2016,
                  strrep("",1:9))

herb_legible <- c(2001,
                 strrep("", 1:3), 
                 2003,
                 strrep("", 1:3),
                 2004,
                 strrep("", 1:5),
                 2006,
                 strrep("", 1:3), 
                 2007,
                 strrep("", 1:7),
                 2008,
                 strrep("", 1:2),
                 2009,
                 strrep("", 1:16),
                 2010,
                 strrep("",1:2),
                 2011,
                 strrep("",1:4),
                 2012,
                 strrep("",1:2),
                 2013,
                 strrep("",1:6),
                 2014,
                 strrep("",1:4),
                 2016,
                 strrep("",1:9))

cma_herb <- viz_forest(x = rma_herb, 
                      #study_labels = effects_herb[, "publicationyear"], 
                      study_labels = herb_legible,
                       xlab = "Response Ratio",
                      summary_col = "Oranges",
                       method = "REML",
                        type = "cumulative") +
                        ggtitle("Herbaceous Plants (N = 79)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_herb

# insect
effects_insect <- filter(ordered_by_year, invasivespeciestaxa == "insect")
effects_insect
rma_insect <- rma(yi=effects_insect$yi, 
                vi=effects_insect$vi,
                method = "REML",
                test = "knha",
                data=effects_insect)
rma_insect
1-exp(-0.5216)
1-(exp(-0.5216)*((sigma(rma_insect)^2)/2))
1.8*.59
1-(exp(-0.2086)*((sigma(rma_tree)^2)/2))

sigma(rma_tree)
sigma(rma_insect)

rma_insect_five <-rma(yi=effects_insect[1:5,]$yi, 
  vi=effects_insect[1:5,]$vi,
method = "REML",
test = "knha",
data=effects_insect[1:5,])
rma_insect_five

plyr::count(effects_insect$publicationyear)
insect_labels <- c(1999,
                 strrep("", 1), 
                 2001,
                 2002,
                 2003,
                 strrep("", 1), 
                 2006,
                 2007,
                 2008,
                 2009,
                 strrep("",1),
                 2010,
                 strrep("",1:4),
                 2011,
                 strrep("",1),
                 2013,
                 strrep("",1),
                 2015,
                 strrep("",1:5),
                 2016)

cma_insect <- viz_forest(x = rma_insect, 
                       #study_labels = effects_insect[, "publicationyear"], 
                       study_labels = insect_labels,
                       xlab = "Response Ratio",
                       summary_col = "Oranges",
                       method = "REML",
                       type = "cumulative") +
                      ggtitle("Insects (N = 27)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))

cma_insect
cma_insect$data
#mean effect
mean(c(-1.5943599,-0.8586897))
# mean low ci
mean(c(-2.8914857,-2.0637464))
# mean high ci
mean(c(-0.29723403,0.34636708))
# mammal
effects_mammal <- filter(ordered_by_year, invasivespeciestaxa == "mammal")
fsn(yi, vi, data=effects_mammal)

rma_mammal <- rma(yi=effects_mammal$yi, 
                vi=effects_mammal$vi,
                method = "REML",
                test = "knha",
                data=effects_mammal)
rma_mammal

plyr::count(effects_mammal$publicationyear)
mammal_labels <- c(2004,
                 strrep("", 1:2),
                 2006,
                 2007,
                 2009,
                 2010,
                 2016,
                 strrep("",1:8))

cma_mammal <- viz_forest(x = rma_mammal, 
                         #study_labels = effects_mammal[, "publicationyear"], 
                         study_labels = mammal_labels,
                         xlab = "Respone Ratio",
                         summary_col = "Oranges",
                         method = "REML",
                         type = "cumulative") +
                        ggtitle("Mammals (N = 16)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_mammal

# molluscs
effects_molluscs <- filter(ordered_by_year, invasivespeciestaxa == "molluscs")
fsn(yi, vi, data=effects_molluscs)

rma_molluscs <- rma(yi=effects_molluscs$yi, 
                  vi=effects_molluscs$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_molluscs)
rma_molluscs

plyr::count(effects_molluscs$publicationyear)
moll_labels <- c(2008,
                 2009,
                 2013,
                 2014,
                 strrep("",1:2),
                 2016)

cma_molluscs <- viz_forest(x = rma_molluscs, 
                           #study_labels = effects_molluscs[, "publicationyear"], 
                           study_labels = moll_labels,
                           xlab = "Response Ratio",
                           summary_col = "Oranges",
                           method = "REML",
                           type = "cumulative") +
                          ggtitle("Mollusks (N = 7)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_molluscs

# tree
effects_tree <- filter(ordered_by_year, invasivespeciestaxa == "tree")
fsn(yi, vi, data=effects_tree)

rma_tree <- rma(yi=effects_tree$yi, 
                  vi=effects_tree$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_tree)
rma_tree
1-(exp(-0.2086)*((sigma(rma_tree)^2)/2))
1-exp(-.2086)
# tree first five
rma_tree_first_five <- rma(yi=effects_tree[1:5,]$yi, 
                           vi=effects_tree[1:5,]$vi,
                           method = "REML",
                           test = "knha",
                           data=effects_tree[1:5,])
rma_tree_first_five
rma_tree_first_five$b
1-exp(-.3885)

1-(exp(-.3885)*((sigma(rma_tree_first_five)^2)/2))

1-exp(-.3885)*((sigma(rma_tree_first_five)^2)/2)

plyr::count(effects_tree$publicationyear)
tree_labels <- c(2000,
                 2001,
                 2002,
                 2003,
                 strrep("", 1:2),
                 2004,
                 strrep("", 1:2),
                 2005,
                 strrep("",1),
                 2006,
                 2007,
                 strrep("", 1:7),
                 2008,
                 strrep("", 1:9),
                 2009,
                 strrep("", 1:5),
                 2010,
                 strrep("",1:2),
                 2011,
                 strrep("",1:2),
                 2012,
                 strrep("",1:6),
                 2013,
                 strrep("",1:6),
                 2014,
                 strrep("",1:19),
                 2015,
                 strrep("",1:6),
                 2016,
                 strrep("",1:4))

tree_labels
tree_legible <- c(2000,
                 strrep("", 1),
                 strrep("", 1),
                 2003,
                 strrep("", 1:2),
                 2004,
                 strrep("", 1:2),
                 2005,
                 strrep("",1:2),
                 2007,
                 strrep("", 1:7),
                 2008,
                 strrep("", 1:9),
                 2009,
                 strrep("", 1:5),
                 2010,
                 strrep("",1:2),
                 2011,
                 strrep("",1:2),
                 2012,
                 strrep("",1:6),
                 2013,
                 strrep("",1:6),
                 2014,
                 strrep("",1:19),
                 2015,
                 strrep("",1:6),
                 2016,
                 strrep("",1:4))

cma_tree <- viz_forest(x = rma_tree,
                      #study_labels = effects_tree[, c("publicationyear")], 
                      study_labels = tree_legible,
                      xlab = "Response Ratio",
                      summary_col = "Oranges",
                      method = "REML",
                      type = "cumulative") +
                      ggtitle("Trees (N = 88)") +
  theme(plot.title = element_text(hjust=0.5, size = 14, colour = "black"),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"))
cma_tree

# Combine all CMAs with more than 10 studies
# plot_grid(cma_tree,cma_herb,cma_grass,cma_insect,cma_crust,cma_algae,cma_fish,cma_mammal,cma_aquatic_plants,cma_molluscs,ncol=5)
plot_grid(cma_tree,cma_insect,cma_algae, labels = c('A', 'B','C'), ncol = 3)
dev.off()

