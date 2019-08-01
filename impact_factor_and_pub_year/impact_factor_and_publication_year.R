## READ IN DATA ####
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(metaviz)
library(metafor)
library(gridExtra)
library(lme4)

# Get subset of columns for CMA
impact_factor_model <- select(raw_data_imputed, code, publicationyear,impactfactor)
dim(impact_factor_model)
distinct_articles <- distinct(impact_factor_model)
dim(distinct_articles)
View(distinct_articles)
# make model for impact factor
linear_model_impact_factor <- lmer(impactfactor ~ publicationyear + (1|code), data=distinct_articles)  # build linear regression model on full data
summary(linear_model_impact_factor)

#### Creating figure for linear model and journal rank
impact_factor_plot <- ggplot(data = distinct_articles,
                             aes(x = publicationyear,
                                 y = impactfactor)) +
  geom_point(size = 2,
             alpha = .4,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = TRUE,
    size = 1,
    alpha = .5
  ) +
  theme_cowplot() +
  ylab("SCImago Journal Rank") +
  xlab("Publication year") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
impact_factor_plot
pdf(file="~/Desktop/CH3_impacts_meta_analysis/figures/linear_model_impact_factor.pdf")
impact_factor_plot
dev.off()
dev.off()

