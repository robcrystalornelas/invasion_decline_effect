source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

library(dplyr)

head(raw_data_imputed)

species_and_taxa <- select(raw_data_imputed, invasivespecies, invasivespeciestaxa)
unique_taxa <- distinct(species_and_taxa)
unique_taxa

counted <- plyr::count(unique_taxa$invasivespeciestaxa)
sum(counted$freq)
