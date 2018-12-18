library(countrycode)
library(dplyr)

## Clean data

# Add a column w/ country code
source("~/Desktop/CH3_impacts_meta_analysis/scripts/ch_3_raw_data.R")
head(raw_data_imputed)
code_and_country <- select(raw_data_imputed, code, country)

# Data frame with unique ID and country
head(code_and_country)
country_code_df <- countrycode(as.character(code_and_country$country), origin = "country.name", destination = "iso3n", warn = TRUE)
class(country_code_df)

# join the country code vector to original data.frame
head(country_code_df)
# country_code_df <- dplyr::rename(country_code_df, isocode = .) # columns need to have same name, new name first
raw_data_imputed$isocountrycode <- country_code_df
head(raw_data_imputed)

country_code_df_2 <- countrycode(as.character(code_and_country$country), origin = "country.name", destination = "country.name", warn = TRUE)

write.csv(raw_data_imputed, file = "/Users/rpecchia/Desktop/CH3_impacts_meta_analysis/scripts/raw_data_with_ISO.csv")

