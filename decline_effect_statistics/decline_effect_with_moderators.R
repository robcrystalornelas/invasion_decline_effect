# Decline effect linear models w/ lme4
# source code: https://www.rensvandeschoot.com/tutorials/lme4/

## LOAD PACKAGES ####
library(metafor)
library(metaviz)
library(ggplot2)
library(lme4)
library(lmerTest) # lme4 doesn't give p-values, but this package adds them
library(cowplot)

# Read in data
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

## Check the data we are using
dim(raw_data_imputed)

# Get effect sizes for all studies in the database
# The output here, or the ROM, is the log-transformed ratio of means
effect_sizes_richness_imputed <- escalc("ROM", # Specify the outcome that we are measuing, RD, RR, OR, SMD etc.
                                        m1i = raw_data_imputed$mean_invaded,       
                                        n1i = raw_data_imputed$sample_size_invaded, # Then, follow with all of the columns needed to compute SMD
                                        sd1i = raw_data_imputed$SD_invaded, 
                                        m2i = raw_data_imputed$mean_control,
                                        n2i = raw_data_imputed$sample_size_control, 
                                        sd2i = raw_data_imputed$SD_control,
                                        data = raw_data_imputed)



#### using the lme4 tutorials combined with the iacarella method
head(effect_sizes_richness_imputed)

# First, create an intercept only model
interceptonlymodel <- lmer(yi~1 + (1|code), data=effect_sizes_richness_imputed) #to run the model
# In the above model, yi is the dependent variable we want to predict.
# the first one indicates the intercept
# No other independent variable since is the intercept only model
# (1|code) assigns random error term to studies by their unique code
summary(interceptonlymodel) # This gives us parameter estimates
# 0.2183 is the residual variance on the "study" level

# Model 1 is for full decline effect, with no moderators
model1 <- lmer(yi~1 + publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model1)

# Model 2 is for interaction of study location and publication year
effect_sizes_richness_imputed$island_or_continent <- relevel(effect_sizes_richness_imputed$island_or_continent, ref = "island")
model2 <- lmer(yi~ 1 + island_or_continent*publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model2)

# Model 3 is interaction between trophic and publication year
model3 <- lmer(yi~ 1 + invasive_trophic_position*publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model3)

# Model 4 is interaction of SCImago Journal rank with publication year
impact_factor_model <- select(raw_data_imputed, code, publicationyear,impactfactor)
dim(impact_factor_model)
distinct_articles <- distinct(impact_factor_model)
distinct_articles
model4 <- lmer(impactfactor ~ publicationyear + (1|code), data = distinct_articles)
summary(model4)

# Model 5 is interaction of taxa with publication year
effect_sizes_select_taxa <-
  effect_sizes_richness_imputed %>% filter(
    invasivespeciestaxa == "tree" |
      invasivespeciestaxa == "fish" |
      invasivespeciestaxa == "insect" |
      invasivespeciestaxa == "algae and seaweed" |
      invasivespeciestaxa == "herbaceous plant" |
      invasivespeciestaxa == "grasses" |
      invasivespeciestaxa == "crustacean" |
      invasivespeciestaxa == "mammal" |
      invasivespeciestaxa == "aquatic plant"
  ) %>%
  droplevels()
linear_model_select_taxa <- lmer(yi ~ 1 + invasivespeciestaxa*publicationyear + (1|code), data = effect_sizes_select_taxa)
summary(linear_model_taxa)

effect_sizes_richness_imputed
effect_sizes_select_taxa_smaller <-
  effect_sizes_richness_imputed %>% filter(
    invasivespeciestaxa == "tree" |
      invasivespeciestaxa == "insect" |
      invasivespeciestaxa == "algae and seaweed" |
      invasivespeciestaxa == "mammal")

linear_model_taxa_smaller <- lmer(yi ~ 1 + invasivespeciestaxa + (1|code), data = effect_sizes_select_taxa_smaller)  # build linear regression model on full data
summary(linear_model_taxa_smaller)


## Model for interaction between invasion history and publication year
time_since_invasion_data <- na.omit(effect_sizes_richness_imputed$time_since_invasion)

time_since_invasion_complete <- effect_sizes_richness_imputed[!is.na(effect_sizes_richness_imputed$time_since_invasion),]
dim(time_since_invasion_complete)


# Get median time since invasion within dataset
median(time_since_invasion_complete$time_since_invasion)

# Bin data into either short time since invasion or long
time_since_invasion_complete$time_since_invasion_binned <- rep("NA")
time_since_invasion_complete$time_since_invasion_binned
time_since_invasion_complete$time_since_invasion_binned[time_since_invasion_complete$time_since_invasion<= 63] <- "short"
time_since_invasion_complete$time_since_invasion_binned[time_since_invasion_complete$time_since_invasion>63] <- "long"
time_since_invasion_complete$time_since_invasion_binned

linear_model_time_since_invasion <- lmer(yi ~ 1 + time_since_invasion_binned*publicationyear + (1|code), data = time_since_invasion_complete)  # build linear regression model on full data
summary(linear_model_time_since_invasion)

#### Creating figure for linear model and journal rank
impact_factor_plot <- ggplot(data = distinct_articles,
                             aes(x = publicationyear,
                                 y = impactfactor)) +
  geom_point(
    size = 1.5,
    alpha = .8,
    position = "jitter",
    color = "#238A8DFF"
  ) +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8,
    color = "#238A8DFF"
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

# Figure for trophic position
trophic_position_plot <-
  ggplot(data = effect_sizes_richness_imputed,
         aes(
           x = publicationyear,
           y = yi,
           col = as.factor(invasive_trophic_position)
         )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = 1,
             alpha = .8,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Publication year") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  labs(col = "Trophic position") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
trophic_position_plot

# Figure for island vs. continent
study_location_plot <- ggplot(data = effect_sizes_richness_imputed,
                              aes(
                                x = publicationyear,
                                y = yi,
                                col = as.factor(island_or_continent)
                              )) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") +
  geom_point(size = 1,
             alpha = .7,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Publication year") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  labs(col = "Study location") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
study_location_plot

# Figure for short vs. long
invasion_history_plot <-
  ggplot(data = time_since_invasion_complete,
         aes(
           x = publicationyear,
           y = yi,
           col = as.factor(time_since_invasion_binned)
         )) +
  geom_point(size = 1,
             alpha = .7,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Publication year") +
  scale_discrete_manual(aesthetics = "colour",
                        values = c("#73D055FF", "#39568CFF")) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  labs(col = "Invasion history") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
invasion_history_plot

### plot for invasive species taxa
invasive_species_taxa_plot <-
  ggplot(data = effect_sizes_richness_imputed,
         aes(
           x = publicationyear,
           y = yi,
           col = as.factor(invasivespeciestaxa)
         )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = 1,
             alpha = .8,
             position = "jitter") +
  geom_smooth(
    method = lm,
    se = FALSE,
    size = 1,
    alpha = .8
  ) +
  theme_cowplot() +
  ylab("ln(Response ratio)") +
  xlab("Publication year") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  ) +
  labs(col = "Invasive species taxa") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    size = .3
  )
invasive_species_taxa_plot

plot_grid(impact_factor_plot, trophic_position_plot, study_location_plot, invasion_history_plot, labels = "AUTO",ncol =2)

View(effect_sizes_richness_imputed)
