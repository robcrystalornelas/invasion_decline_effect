source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(dplyr)       
library(tidyr)       
library(scales)      # labeling
library(ggthemes)   
library(viridis)     # Color palette
library(knitr)   

# Prep data for heatmap
# Add new column with years binned
# Convert data.frame to table
imputed_raw_table <- tbl_df(raw_data_imputed)
plyr::count(raw_data_imputed$ecosystemforheatmap)
## count of all ecosystems and year combinations
ecosystems_and_year_for_heatmap <- dplyr::count(imputed_raw_table, publicationyear, ecosystemforheatmap)
ecosystems_and_year_for_heatmap
# Remove impacts with impact listed as "NA"
all_years_and_ecosystems_complete <-
  ecosystems_and_year_for_heatmap %>%
  filter(ecosystemforheatmap != "NA") %>%
  droplevels() %>%
  tidyr::complete(publicationyear, ecosystemforheatmap, fill=list(n=NA)) # complete heatmap so that all combinations have either numerical value or NA
all_years_and_ecosystems_complete


all_years_and_ecosystems_complete_ordered <- all_years_and_ecosystems_complete
all_years_and_ecosystems_complete_ordered$ecosystemforheatmap <- factor(all_years_and_ecosystems_complete_ordered$ecosystemforheatmap, ordered = TRUE,
                                                                        levels = c("mountain","desert","ocean","shrubland","urban","multiple","intertidal","coastal","lentic","estuarine","island","grassland","lotic","forest"))
                                                                       
                                                                        #levels = c('forest', 'lotic', 'grassland', 'island', 'estuarine', 'lentic', 'coastal',"intertidal","multiple","urban","shrubland","ocean","desert","mountain"))
# Make the heatmap
gg <- ggplot(all_years_and_ecosystems_complete_ordered, aes(x=publicationyear, y=ecosystemforheatmap, fill = n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg
gg <- gg + scale_fill_viridis(option = "C", name="# of case studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=18, angle = 90))
gg <- gg + theme(axis.text.y=element_text(size=18))
gg <- gg + theme(axis.title = element_text(size=20))
gg <- gg + ylab("Ecosystem")
gg <- gg + xlab("Publication Year")
gg

pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/heatmap_year_and_ecosystem.pdf")
gg
dev.off()
dev.off()
