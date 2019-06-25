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
# 0.2667 is the residual variance on the "study" level

# OK, so now let's add in a first level predictor.
# Add in the FIXED EFFECTS predictor of year
model1 <- lmer(yi~1 + publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model1)

# all predictors: year, trophic, islandorcontinent, impact factor
model5 <- lmer(yi~1 + publicationyear + invasive_trophic_position + island_or_continent + impactfactor + (1|code), data = effect_sizes_richness_imputed)
summary(model5)

# Interaction between publication year and trophic position
# the interpretation of this model is that we have publication year as a main effect, and then we look
# at the interaction between publication within trophic positions
model7 <- lmer(yi~ 1 + publicationyear + invasive_trophic_position:publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model7)

model8 <- lmer(yi~ 1 + publicationyear + island_or_continent:publicationyear + (1|code), data = effect_sizes_richness_imputed)
summary(model8)

# Interation between pub year and trophic
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
AIC(model7)
AIC(model8)

## Example orange data
nlmer(
  circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym |
    Tree,
  Orange,
  start = c(Asym = 200,xmid = 770, scal = 120)
)
Orange
?nlmer
### Try out some non-linear mixed-effects models
nlmer(yi ~ SSlogis(sample_size_control) ~ publicationyear + code,
      data = effect_sizes_richness_imputed)
head(effect_sizes_richness_imputed)

# Figure for trophic position
trophic_position_plot <- ggplot(data = effect_sizes_richness_imputed, 
       aes(
         x = publicationyear,
         y = yi,
         col = as.factor(invasive_trophic_position)
       )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = .8,
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
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=13)) + 
  labs(col = "Trophic position") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=.3)
trophic_position_plot

# Figure for island vs. continent
study_location_plot <- ggplot(data = effect_sizes_richness_imputed, 
       aes(
         x = publicationyear,
         y = yi,
         col = as.factor(island_or_continent)
       )) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(size = .8,
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
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text=element_text(size=13)) + 
  labs(col = "Study location") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=.3)
study_location_plot

plot_grid(study_location_plot, trophic_position_plot, labels = "AUTO", align = "h")

