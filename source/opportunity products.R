rm(list = ls())

library(readr)
library(dplyr)
library(readxl)

products_hs2 <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 2,
         Classification == "H4") %>%
  select(Code, Description, `Code Parent`) %>%
  rename(hs2_code = Code,
         hs2_desc = Description,
         hs1_code = `Code Parent`) %>%
  distinct(hs2_code, .keep_all = TRUE) %>%
  select(-hs1_code)

products_hs4 <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 4) %>%
  select(Code, Description, `Code Parent`) %>%
  rename(hs4_code = Code,
         hs4_desc = Description,
         hs2_code = `Code Parent`) %>%
  distinct(hs4_code, .keep_all = TRUE)

products <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 6) %>%
  select(Code, Description, `Code Parent`) %>%
  rename(product = Code,
         parent_code = `Code Parent`,
         description = Description) %>%
  distinct(product, .keep_all = TRUE)

opp_products <- read_csv("./data/country_hsproduct4digit_year.csv")

# 1248 unique products (HS4)
opp_products %>%
  distinct(hs_product_code) %>%
  nrow()

opp_products_dom <- opp_products %>%
  select(year, location_code, hs_product_code, normalized_distance,
         normalized_pci, product_status) %>%
  filter(location_code == "DOM",
         hs_product_code != "XXXX",
         hs_product_code != "9999") %>%
  group_by(year) %>%
  mutate(rank = rank(-normalized_distance)) %>%
  ungroup() %>%
  filter(rank <= 50) %>%
  left_join(products_hs4,
            by = c("hs_product_code" = "hs4_code")) %>%
  left_join(products_hs2,
            by = c("hs2_code"))


rm("opp_products")

write_csv(opp_products_dom, "./data/processed/opportunity_products.csv")

trade_2019 <- read_csv("./data/BACI_HS92_Y2019_V202102.csv",
                       col_types = "dcccdd") %>%
  rename(year = t,
         product = k,
         importer = j,
         exporter = i,
         export_val = v,
         export_qua = q) %>%
  left_join(products)
