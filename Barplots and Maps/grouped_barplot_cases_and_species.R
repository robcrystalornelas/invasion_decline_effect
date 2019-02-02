## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(arrange)
library(tidyverse)
library(viridis)
# Count up how many case studies were produced each year
cases_and_publication_year <- dplyr::select(raw_data_imputed, code, publicationyear)

counted_cases <- cases_and_publication_year %>%
  count(publicationyear) %>%
  add_column("group" = rep("cases"))
counted_cases  

# count up how many species were studied each year
count_species_by_year <- dplyr::select(raw_data_imputed, code, publicationyear, invasivespecies)
counted_species_for_cases <- count_species_by_year %>%
  group_by(publicationyear) %>%
  summarise(n_distinct(invasivespecies)) %>%
  add_column("group" = rep("species"))
counted_species_for_cases
counted_species_for_cases <- rename(counted_species_for_cases, n = `n_distinct(invasivespecies)`)
counted_species_for_cases

# Join up two tibbles
joined_species_and_cases <- full_join(counted_species_for_cases,counted_cases)
joined_species_and_cases

# Make grouped barplot
# Make a grouped barplot
gg<- ggplot(joined_species_and_cases, aes(fill = group, y = n, x = publicationyear)) +
  geom_bar(position = "dodge", stat = "identity")
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Publication Year")
gg <- gg + scale_fill_manual(values=c("coral3","darkcyan"))
gg <- gg + theme(axis.text.x = element_text(size=30, angle = 90, vjust = 0.5), # Size 30 works best for presentations
                 axis.text.y = element_text(size=30),
                 axis.title = element_text(size=30)) +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 15))


gg
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/grouped_barplot_species_and_cases.pdf")
gg
dev.off()
dev.off()
