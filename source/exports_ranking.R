rm(list = ls())
options(scipen = 999)

library(readr)
library(dplyr)
library(readxl)

setwd("./data/country_partner_hs6_hs92")

create <- function(year){
  base_file <- "country_partner_hsproduct6digit_year_"
  file <- paste0(base_file, year, ".csv")

  print("\t Importing trade database.")
  exports <- read_csv(file,
                      col_types = cols(year = col_integer(),
                                       export_value = col_double(),
                                       import_value = col_double(),
                                       hs_eci = col_double(),
                                       hs_coi = col_double(),
                                       .default = col_character()))

  print("\t Creating products and sectors database.")
  prods <- read_tsv("hs_product.tab",
                    col_types = cols(.default = col_character())) %>%
    select(product_id, hs_product_code, hs_product_name_short_en, parent_id)

  products <- exports %>%
    select(hs_product_code) %>%
    unique() %>%
    arrange(hs_product_code) %>%
    left_join(prods, by = "hs_product_code") %>%
    select(-c("product_id")) %>%
    left_join(prods, by = c("parent_id" = "product_id")) %>%
    select(-c("parent_id")) %>%
    left_join(prods, by = c("parent_id.y" = "product_id")) %>%
    select(-c("parent_id.y")) %>%
    left_join(prods, by = c("parent_id" = "product_id")) %>%
    select(-c("parent_id.y", "parent_id"))

  names(products) <- c("hs6_code", "hs6_name", "hs4_code", "hs4_name",
                       "subsector_code", "subsector_name", "sector_code",
                       "sector_name")

  print("\t Creating product ranking.")
  # Exports from DOM to USA, by product and year
  DOM_to_USA <- exports %>%
    filter(location_code == "DOM",
           partner_code == "USA") %>%
    group_by(year, hs_product_code) %>%
    summarise(export_value = sum(export_value)) %>%
    ungroup() %>%
    arrange(-export_value) %>%
    mutate(rank = order(export_value, decreasing = TRUE))

  # Imports to USA from anywhere else, by product and year
  WORLD_to_USA <- exports %>%
    select(-year) %>%
    filter(location_code == "USA") %>%
    group_by(hs_product_code) %>%
    summarise(import_value = sum(import_value)) %>%
    ungroup() %>%
    arrange(-import_value)

  data <- DOM_to_USA %>%
    left_join(WORLD_to_USA, by = "hs_product_code") %>%
    mutate(ab = export_value / import_value) %>%
    left_join(products, by = c("hs_product_code" = "hs6_code"))

  print(paste0("\t Year ", year, " is done"))

  return(data)
}

years <- 1995:2019
data <- tibble::tibble(year = integer(),
                   hs_product_code = character(),
                   export_value = numeric(),
                   rank = integer(),
                   import_value = numeric(),
                   ab = numeric(),
                   hs6_name = character(),
                   hs4_code = character(),
                   subsector_code = character(),
                   subsector_name = character(),
                   sector_code = character(),
                   sector_name = character())

for (year in years){
  print(paste0("working on year ", year))

  data <- data %>%
    rbind(create(year))
}

write_csv(data, "../processed/ranking_exports.csv")
