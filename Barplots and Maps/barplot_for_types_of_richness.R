## Load libraries ####
library(ggplot2)
library(ggthemes)

## Load data ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## ORGANIZE DATA ####
head(raw_data)
richness_type <- dplyr::select(raw_data, richness_of_all_or_native)

## MAKE FIGURES ####
gg <- ggplot(richness_type, aes(x = reorder(richness_of_all_or_native,richness_of_all_or_native, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "forestgreen")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Richness origin")
gg
gg <- gg + theme(axis.text=element_text(size=25), # Change tick mark label size
                 axis.title=element_text(size=25,face="bold"),
                 axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
                 strip.text = element_text(size=25)) # Change axis title size

gg

gg
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/barplot_richness_origin.pdf")
gg
dev.off()
dev.off()
