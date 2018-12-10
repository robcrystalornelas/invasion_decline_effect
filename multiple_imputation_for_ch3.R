library(lattice)
library(mice)
library(dplyr) 
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")

head(raw_data_with_na)
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
set.seed(103)
imputed_sds = mice(imputation_subset_with_na, method=meth, predictorMatrix=predM, m=5)

# Create a dataset after the imputation
imputed <- complete(imputed_sds)

# Do we still havy missing values? Hopefully not!
sapply(imputed_sds, function(x) sum(is.na(x)))

tail(imputed_sds, n = 20)
