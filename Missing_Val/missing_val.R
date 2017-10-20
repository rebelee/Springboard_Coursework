library(tidyr)
library(dplyr)

# load data into RStudio
titanic_original <- read.csv("titanic_original.csv")

# replace missing values in the embarked column with S
titanic <- titanic_original %>%
  mutate(embarked = gsub("^$", "S", embarked))

# fill missing values in age with the mean
titanic <- titanic %>%
  mutate(age = ifelse(is.na(age), mean(titanic$age, na.rm = TRUE), age))

# fill missing values in lifeboats with a dummy value, e.g. None or NA
titanic <- titanic %>%
  mutate(boat = gsub("^$", "NA", boat))

# create columns to indicate survival based on cabin number values
titanic <- titanic %>%
  mutate(has_cabin_number = ifelse(grepl("^$", cabin), 0, 1))

write.csv(titanic, file = "titanic_clean.csv")
