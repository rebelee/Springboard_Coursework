library(tidyr)
library(dplyr)

# load data into RStudio
refine_original <- read.csv("refine_original.csv")
#View(refine_original)

# clean up brand names
refine <- refine_original %>%
  mutate(company = gsub("^p.*S$|^f.*s$", "philips", company, ignore.case = TRUE) %>%
                   gsub("^a.*o$|^a.*0$", "akzo", ., ignore.case = TRUE) %>%
                   gsub("^v.*en$", "van houten", ., ignore.case = TRUE) %>%
                   gsub("^u.*r$", "unilever", ., ignore.case = TRUE))

# separate product code and number
refine <- refine %>%
  separate(Product.code...number, c("product_code", "product_number"), sep = "-")

# add product categories
refine <- refine %>%
  mutate(product_category = gsub("p", "Smartphone", product_code) %>%
                            gsub("v", "TV", .) %>%
                            gsub("x", "Laptop", .) %>%
                            gsub("q", "Tablet", .))

# add full address for geocoding
refine <- refine %>%
  unite("full_address", address, city, country, sep = ",")

# create dummy variables for company and product category
refine <- refine %>%
  mutate(company_philips = ifelse(company == "philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0)) %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate(product_tv = ifelse(product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))

write.csv(refine, file = "refine_clean.csv")