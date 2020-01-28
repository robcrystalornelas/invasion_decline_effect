## LOAD PACKAGES ####
library(dplyr)
library(lattice)
library(mice)

## READ IN DATA ####
## Imputation for missing values, adds approx 15 case studies ####
raw_data_with_na <- read.csv("~/Desktop/research/CH3_impacts_meta_analysis/diversity_cases_working_file_v9.csv", header=TRUE)

head(raw_data_with_na)
tail(raw_data_with_na)
imputation_subset_with_na <- dplyr::select(raw_data_with_na, code, mean_control, SD_control,sample_size_control,mean_invaded,SD_invaded,sample_size_invaded)
imputation_subset_with_na
dim(imputation_subset_with_na)

imputation_subset_with_na <- imputation_subset_with_na %>%
  mutate(
    code = as.factor(code),
    mean_control = as.numeric(mean_control),
    SD_control = as.numeric(SD_control),
    sample_size_control = as.numeric(sample_size_control),
    mean_invaded = as.numeric(mean_invaded),
    SD_invaded = as.numeric(SD_invaded),
    sample_size_invaded = as.numeric(sample_size_invaded)
  )

# Here's some boiler plate code for imputation
init <- mice(imputation_subset_with_na, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("mean_control","mean_invaded","sample_size_control","sample_size_invaded")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("SD_control","SD_invaded")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds <- mice(imputation_subset_with_na, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds)

# Create a dataset after the imputation
imputed_sds <- mice::complete(imputed_sds)
imputed_sds
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds, function(x) sum(is.na(x)))

tail(imputed_sds, n = 20)

# Now that we have imputed values, replace whole column in R
raw_data_with_na$SD_control <- imputed_sds$SD_control
raw_data_with_na$SD_invaded <- imputed_sds$SD_invaded

raw_data_imputed <- raw_data_with_na
tail(raw_data_imputed)
dim(raw_data_imputed) # This is number of case studies
unique(raw_data_imputed$code)

