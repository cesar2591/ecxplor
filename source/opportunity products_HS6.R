rm(list = ls())

library(readr)
library(dplyr)
library(readxl)

# Correspondence table extracted from
# https://exportpotential.intracen.org/en/resources/data/correspondences
products <- read_xlsx("./data/ITC_EPM-Full_correspondences_en.xlsx") %>%
  select(k, k_name_short, k_name, Sub_sector_name, Sector_name) %>%
  unique()

# Exports data with EC indexes, extracted from
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T4CHWJ&version=4.0
opp_products <- read_csv("./data/country_hsproduct4digit_year.csv")

# 1,248 unique products (HS4)
opp_products %>%
  distinct(hs_product_code) %>%
  nrow()

opp_products_dom <- opp_products %>%
  filter(location_code == "DOM",
         hs_product_code != "XXXX",
         hs_product_code != "9999") %>%
  group_by(year) %>%
  mutate(rank = rank(-normalized_distance)) %>%
  ungroup() %>%
  filter(rank <= 50) %>%
  left_join(products,
            by = c("hs_product_code" = "k"))

rm("opp_products")

write_csv(opp_products_dom, "./data/processed/opportunity_products_hs4.csv")
