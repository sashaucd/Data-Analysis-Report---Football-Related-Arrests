# load the dataset
library(readxl)
fb_arrests <- read_xlsx("football-related-arrest-statistics-england-and-wales-detailed-datasets.xlsx", sheet = "Arrests")
fb_arrests
View(fb_arrests)

# Rearrange the arrests data set in chronological order
fb_arrests_arranged <- fb_arrests %>%
  arrange(date)
View(fb_arrests_arranged)

# Monthly gdp uk
monthly_gdp <- read_xlsx("Copy of mgdp (monthly gdp uk).xlsx")
View(monthly_gdp)
monthly_gdp_ammended <- monthly_gdp %>%
  slice(7:n())
View(monthly_gdp_ammended)

# convert the time variables
library(dplyr)
library(lubridate)

# Convert time variables to a common format (in this case, using lubridate's 'ym' function)
fb_arrests_arranged$time_variable <- (fb_arrests_arranged$date)
monthly_gdp_ammended$time_variable <- dmy(monthly_gdp_ammended$Title)

# join the datasets on the common time variable
joined_df <- left_join(df1, df2, by = "time_common")

# load the new data set
setwd('/Users/jackoconnell/Desktop/ADWV')
library(readxl)
fb_arrests_monthly_gdp <- read_xlsx("fb_arrests_monthly_gdp.xlsx")
View(fb_arrests_monthly_gdp)


