## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)

## MAKE FIGURES ####
# Barplot for number of publications by year for ARTICLES
# Fist, get the two rows that will show use unique publications
code_and_publication_year <- dplyr::select(raw_data, code, publicationyear)
distinct_code_and_publication_year <- distinct(code_and_publication_year)

# Make the histogram with ggplot for UNIQUE PUBLICATIONS
binsize <- diff(range(distinct_code_and_publication_year$publicationyear))/17 #set to a total of 17 bins, one for each year

gg <- ggplot(distinct_code_and_publication_year, aes(publicationyear)) + 
  geom_histogram(binwidth = binsize, fill = "deepskyblue3", colour = "white")
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Publication Year")
gg <- gg + theme(axis.text.x = element_text(size=30), # Size 30 works best for presentations
      axis.text.y = element_text(size=30),
      axis.title = element_text(size=30))
gg
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/ch_3_publications_by_year.pdf")
png(file="~/Desktop/CH3_impacts_meta_analysis/figures/ch_3_publications_by_year.png")
gg
dev.off()
dev.off()
