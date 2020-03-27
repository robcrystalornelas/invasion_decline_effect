## READ IN DATA ####
source("~/Desktop/research/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggthemes)
library(arrange)
library(tidyverse)

## MAKE FIGURES ####
# Barplot for number of publications by year for ARTICLES
# Fist, get the two rows that will show use unique publications
code_and_publication_year <- dplyr::select(raw_data_imputed, code, publicationyear)
distinct_code_and_publication_year <- distinct(code_and_publication_year)

# Make the histogram with ggplot for UNIQUE PUBLICATIONS
binsize <- diff(range(distinct_code_and_publication_year$publicationyear))/17 #set to a total of 17 bins, one for each year
counted_articles <- distinct_code_and_publication_year %>%
  count(publicationyear) %>%
  add_column("group" = rep("articles"))
counted_articles  

gg <-
  ggplot(distinct_code_and_publication_year, aes(publicationyear)) +
  geom_histogram(binwidth = binsize,
                 fill = "deepskyblue3",
                 colour = "white")
gg <- gg + theme_cowplot()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Publication Year")
gg <- gg + scale_y_continuous(expand = c(0,0))
gg <-
  gg + theme(
    axis.text.x = element_text(size = 30),
    # Size 30 works best for presentations
    axis.text.y = element_text(size = 30),
    axis.title = element_text(size = 30)
  )
gg
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/barplot_publications_by_year.pdf")
gg
dev.off()


# How many unique species are studied per year?
head(raw_data_imputed)
count_species_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies)

distinct_code_and_publication_year <- distinct(code_and_publication_year)

# How many unique species are studied each year?
counted_species <- count_species_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasivespecies)) %>%
  add_column("group" = rep("species"))
counted_species
counted_species <- rename(counted_species, n = `n_distinct(invasivespecies)`)
counted_species

joined_species_and_articles <- full_join(counted_species,counted_articles)
joined_species_and_articles

# Make a grouped barplot
gg<- ggplot(joined_species_and_articles, aes(fill = group, y = n, x = publicationyear)) +
  geom_bar(position = "dodge", stat = "identity")
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Publication Year")
gg <- gg + scale_fill_manual(values=c("#13306dff","#de7065ff"))
gg <- gg + theme(axis.text.x = element_text(size=30, angle = 90, vjust = 0.5), # Size 30 works best for presentations
                 axis.text.y = element_text(size=30),
                 axis.title = element_text(size=30)) +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 15))


gg

pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/grouped_barplot_species_and_articles.pdf")
gg
dev.off()
dev.off()

