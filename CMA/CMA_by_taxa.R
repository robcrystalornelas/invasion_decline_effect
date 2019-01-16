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

# Only do CMAs on taxa with more than 10 cases ####
plyr::count(ordered_by_year$invasivespeciestaxa)
# Algae and seaweed
effects_algae <- filter(ordered_by_year, invasivespeciestaxa == "algae and seaweed")
effects_algae

rma_algae <- rma(yi=effects_algae$yi, 
                 vi=effects_algae$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_algae)
rma_algae

cma_algae <- viz_forest(x = rma_algae, 
                        study_labels = effects_algae[, "publicationyear"], 
                        method = "REML",
                        xlab = "Response Ratio",
                        variant = "thick",
                        type = "cumulative") +
                        ggtitle("Algae") +
                        theme(plot.title = element_text(hjust=0.5))
cma_algae
  
# aquatic plants
effects_aquatic_plants <- filter(ordered_by_year, invasivespeciestaxa == "aquatic plant")
effects_aquatic_plants

rma_aquatic_plants <- rma(yi=effects_aquatic_plants$yi, 
                 vi=effects_aquatic_plants$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_aquatic_plants)
rma_aquatic_plants

cma_aquatic_plants <- viz_forest(x = rma_aquatic_plants, 
                        study_labels = effects_aquatic_plants[, "publicationyear"], 
                        method = "REML",
                        xlab = "Response Ratio",
                        variant = "thick",
                        type = "cumulative") +
  ggtitle("Aquatic plants") +
  theme(plot.title = element_text(hjust=0.5))
cma_aquatic_plants

# crustacean
effects_crust <- filter(ordered_by_year, invasivespeciestaxa == "crustacean")
effects_crust
rma_crust <- rma(yi=effects_crust$yi, 
                 vi=effects_crust$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_crust)
rma_crust

cma_crust <- viz_forest(x = rma_crust, 
                        study_labels = effects_crust[, "publicationyear"], 
                        xlab = "Response Ratio",
                        method = "REML",
                        variant = "thick",
                        type = "cumulative") +
                        ggtitle("Crustacean") +
                        theme(plot.title = element_text(hjust=0.5))
cma_crust

# fish
effects_fish <- filter(ordered_by_year, invasivespeciestaxa == "fish")
effects_fish
rma_fish <- rma(yi=effects_fish$yi, 
                 vi=effects_fish$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_fish)
rma_fish

cma_fish <- viz_forest(x = rma_fish, 
                        study_labels = effects_fish[, "publicationyear"], 
                        xlab = "Response Ratio",
                       method = "REML",
                        variant = "thick",
                        type = "cumulative") +
                        ggtitle("Fish") +
                        theme(plot.title = element_text(hjust=0.5))
cma_fish

# grasses
effects_grass <- filter(ordered_by_year, invasivespeciestaxa == "grasses")
rma_grass <- rma(yi=effects_grass$yi, 
                vi=effects_grass$vi,
                method = "REML",
                test = "knha",
                data=effects_grass)
rma_grass

cma_grass <- viz_forest(x = rma_grass, 
                       study_labels = effects_grass[, "publicationyear"], 
                       xlab = "Response Ratio",
                       method = "REML",
                       variant = "thick",
                       type = "cumulative") +
                      ggtitle("Grasses") +
                      theme(plot.title = element_text(hjust=0.5))
cma_grass

# herbaceous plant
effects_herb <- filter(ordered_by_year, invasivespeciestaxa == "herbaceous plant")
rma_herb <- rma(yi=effects_herb$yi, 
                 vi=effects_herb$vi,
                 method = "REML",
                 test = "knha",
                 data=effects_herb)
rma_herb

cma_herb <- viz_forest(x = rma_herb, 
                        study_labels = effects_herb[, "publicationyear"], 
                        xlab = "Response Ratio",
                       method = "REML",
                        variant = "thick",
                        type = "cumulative") +
                        ggtitle("Herbaceous Plants") +
                        theme(plot.title = element_text(hjust=0.5))
cma_herb

# insect
effects_insect <- filter(ordered_by_year, invasivespeciestaxa == "insect")
rma_insect <- rma(yi=effects_insect$yi, 
                vi=effects_insect$vi,
                method = "REML",
                test = "knha",
                data=effects_insect)
rma_insect

cma_insect <- viz_forest(x = rma_insect, 
                       study_labels = effects_insect[, "publicationyear"], 
                       xlab = "Response Ratio",
                       method = "REML",
                       variant = "thick",
                       type = "cumulative") +
                      ggtitle("Insects") +
                      theme(plot.title = element_text(hjust=0.5))
cma_insect

# mammal
effects_mammal <- filter(ordered_by_year, invasivespeciestaxa == "mammal")
rma_mammal <- rma(yi=effects_mammal$yi, 
                vi=effects_mammal$vi,
                method = "REML",
                test = "knha",
                data=effects_mammal)
rma_mammal

cma_mammal <- viz_forest(x = rma_mammal, 
                         study_labels = effects_mammal[, "publicationyear"], 
                         xlab = "Response Ratio",
                         method = "REML",
                         variant = "thick",
                         type = "cumulative") +
                        ggtitle("Mammals") +
                        theme(plot.title = element_text(hjust=0.5))
cma_mammal

# molluscs
effects_molluscs <- filter(ordered_by_year, invasivespeciestaxa == "molluscs")
rma_molluscs <- rma(yi=effects_molluscs$yi, 
                  vi=effects_molluscs$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_molluscs)
rma_molluscs

cma_molluscs <- viz_forest(x = rma_molluscs, 
                           study_labels = effects_molluscs[, "publicationyear"], 
                           xlab = "Response Ratio",
                           method = "REML",
                           variant = "thick",
                           type = "cumulative") +
  ggtitle("Mollusks") +
  theme(plot.title = element_text(hjust=0.5))
cma_molluscs

# tree
effects_tree <- filter(ordered_by_year, invasivespeciestaxa == "tree")
rma_tree <- rma(yi=effects_tree$yi, 
                  vi=effects_tree$vi,
                  method = "REML",
                  test = "knha",
                  data=effects_tree)
rma_tree

cma_tree <- viz_forest(x = rma_tree,
                                     study_labels = effects_tree[, c("publicationyear")], 
                                     xlab = "Response Ratio",
                                     variant = "thick",
                                    method = "REML",
                                     type = "cumulative") +
                                    ggtitle("Trees") +
                                    theme(plot.title = element_text(hjust=0.5))
cma_tree

# Combine all CMAs with more than 10 studies
grid.arrange(cma_algae,cma_aquatic_plants,cma_crust,cma_fish,cma_grass,cma_herb,cma_insect,cma_mammal,cma_molluscs, cma_tree,ncol=5)

dev.off()

# Less than 10 cases ####

# amphibians and reptiles
effects_amphib <- filter(ordered_by_year, invasivespeciestaxa == "amphibians and reptiles")
cma_amphib <- viz_forest(x = effects_amphib[, c("yi", "vi")], 
                         study_labels = effects_amphib[, "publicationyear"], 
                         xlab = "Response Ratio",
                         variant = "thick",
                         type = "cumulative")
cma_amphib

# Avian CMA
effects_avian <- filter(ordered_by_year, invasivespeciestaxa == "avian")
effects_avian
cma_avian <- viz_forest(x = effects_avian[, c("yi", "vi")], 
                        study_labels = effects_avian[, "publicationyear"], 
                        xlab = "Response Ratio",
                        variant = "thick",
                        type = "cumulative")
cma_avian

# marine invert
effects_marine_invert <- filter(ordered_by_year, invasivespeciestaxa == "marine invert")
cma_marine_invert <- viz_forest(x = effects_marine_invert[, c("yi", "vi")], 
                                study_labels = effects_marine_invert[, "publicationyear"], 
                                xlab = "Response Ratio",
                                variant = "thick",
                                type = "cumulative")
cma_marine_invert

# terrestrial invert
effects_terrestrial_invert <- filter(ordered_by_year, invasivespeciestaxa == "terrestrial invert")
cma_terrestrial_invert <- viz_forest(x = effects_terrestrial_invert[, c("yi", "vi")], 
                                     study_labels = effects_terrestrial_invert[, "publicationyear"], 
                                     xlab = "Response Ratio",
                                     variant = "thick",
                                     type = "cumulative")
cma_terrestrial_invert



