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
effect_sizes_richness <- escalc("SMD", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                m1i = raw_data$mean_invaded,       
                                n1i = raw_data$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                sd1i = raw_data$SD_invaded, 
                                m2i = raw_data$mean_control,
                                n2i = raw_data$sample_size_control, 
                                sd2i = raw_data$SD_control,
                                data = raw_data)

ordered_by_year <- arrange(effect_sizes_richness, publicationyear)
head(ordered_by_year)
levels(ordered_by_year$invasivespeciestaxa)

# More than 10 cases ####

# Algae and seaweed
effects_algae <- filter(ordered_by_year, invasivespeciestaxa == "algae and seaweed")
effects_algae

cma_algae <- viz_forest(x = effects_algae[, c("yi", "vi")], 
                        study_labels = effects_algae[, "publicationyear"], 
                        xlab = "Hedge's d",
                        variant = "thick",
                        type = "cumulative")
cma_algae

# crustacean
effects_crust <- filter(ordered_by_year, invasivespeciestaxa == "crustacean")
cma_crust <- viz_forest(x = effects_crust[, c("yi", "vi")], 
                        study_labels = effects_crust[, "publicationyear"], 
                        xlab = "Hedge's d",
                        variant = "thick",
                        type = "cumulative")
cma_crust

# fish
effects_fish <- filter(ordered_by_year, invasivespeciestaxa == "fish")
cma_fish <- viz_forest(x = effects_fish[, c("yi", "vi")], 
                        study_labels = effects_fish[, "publicationyear"], 
                        xlab = "Hedge's d",
                        variant = "thick",
                        type = "cumulative")
cma_fish

# grasses
effects_grass <- filter(ordered_by_year, invasivespeciestaxa == "grasses")
cma_grass <- viz_forest(x = effects_grass[, c("yi", "vi")], 
                       study_labels = effects_grass[, "publicationyear"], 
                       xlab = "Hedge's d",
                       variant = "thick",
                       type = "cumulative")
cma_grass

# herbaceous plant
effects_herb <- filter(ordered_by_year, invasivespeciestaxa == "herbaceous plant")
cma_herb <- viz_forest(x = effects_herb[, c("yi", "vi")], 
                        study_labels = effects_herb[, "publicationyear"], 
                        xlab = "Hedge's d",
                        variant = "thick",
                        type = "cumulative")
cma_herb

# insect
effects_insect <- filter(ordered_by_year, invasivespeciestaxa == "insect")
cma_insect <- viz_forest(x = effects_insect[, c("yi", "vi")], 
                       study_labels = effects_insect[, "publicationyear"], 
                       xlab = "Hedge's d",
                       variant = "thick",
                       type = "cumulative")
cma_insect

# mammal
effects_mammal <- filter(ordered_by_year, invasivespeciestaxa == "mammal")
cma_mammal <- viz_forest(x = effects_mammal[, c("yi", "vi")], 
                         study_labels = effects_mammal[, "publicationyear"], 
                         xlab = "Hedge's d",
                         variant = "thick",
                         type = "cumulative")
cma_mammal

# tree
effects_tree <- filter(ordered_by_year, invasivespeciestaxa == "tree")
cma_tree <- viz_forest(x = effects_tree[, c("yi", "vi")], 
                                     study_labels = effects_tree[, "publicationyear"], 
                                     xlab = "Hedge's d",
                                     variant = "thick",
                                     type = "cumulative")
cma_tree

# Combine all CMAs with more than 10 studies
grid.arrange(cma_mammal,cma_tree,cma_insect,cma_herb,cma_grass,cma_fish,cma_crust,cma_algae,ncol=4)
cma_mammal
cma_tree
cma_insect
cma_herb
cma_grass
cma_fish
cma_crust
cma_algae



# Less than 10 cases ####

# amphibians and reptiles
effects_amphib <- filter(ordered_by_year, invasivespeciestaxa == "amphibians and reptiles")
cma_amphib <- viz_forest(x = effects_amphib[, c("yi", "vi")], 
                         study_labels = effects_amphib[, "publicationyear"], 
                         xlab = "Hedge's d",
                         variant = "thick",
                         type = "cumulative")
cma_amphib

# Aquatic plant
effects_aquatic_plant <- filter(ordered_by_year, invasivespeciestaxa == "aquatic plant")
cma_ap <- viz_forest(x = effects_aquatic_plant[, c("yi", "vi")], 
                     study_labels = effects_aquatic_plant[, "publicationyear"], 
                     xlab = "Hedge's d",
                     variant = "thick",
                     type = "cumulative")
cma_ap

# Avian CMA
effects_avian <- filter(ordered_by_year, invasivespeciestaxa == "avian")
effects_avian
cma_avian <- viz_forest(x = effects_avian[, c("yi", "vi")], 
                        study_labels = effects_avian[, "publicationyear"], 
                        xlab = "Hedge's d",
                        variant = "thick",
                        type = "cumulative")
cma_avian

# marine invert
effects_marine_invert <- filter(ordered_by_year, invasivespeciestaxa == "marine invert")
cma_marine_invert <- viz_forest(x = effects_marine_invert[, c("yi", "vi")], 
                                study_labels = effects_marine_invert[, "publicationyear"], 
                                xlab = "Hedge's d",
                                variant = "thick",
                                type = "cumulative")
cma_marine_invert

# molluscs
effects_molluscs <- filter(ordered_by_year, invasivespeciestaxa == "molluscs")
cma_molluscs <- viz_forest(x = effects_molluscs[, c("yi", "vi")], 
                           study_labels = effects_molluscs[, "publicationyear"], 
                           xlab = "Hedge's d",
                           variant = "thick",
                           type = "cumulative")
cma_molluscs

# terrestrial invert
effects_terrestrial_invert <- filter(ordered_by_year, invasivespeciestaxa == "terrestrial invert")
cma_terrestrial_invert <- viz_forest(x = effects_terrestrial_invert[, c("yi", "vi")], 
                                     study_labels = effects_terrestrial_invert[, "publicationyear"], 
                                     xlab = "Hedge's d",
                                     variant = "thick",
                                     type = "cumulative")
cma_terrestrial_invert



