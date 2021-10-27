rm(list = ls())

library(readr)
library(readxl)
library(dplyr)

source("./R/model.R")

exports_oec <- read_csv("./data/country_partner_hsproduct4digit_year_2019.csv",
                        col_types = cols(year = col_integer(),
                                         export_value = col_double(),
                                         import_value = col_double(),
                                         .default = col_character())) %>%
  rename(country = location_code,
         product = hs_product_code,
         export_val = export_value) %>%
  group_by(country, product) %>%
  summarise(export_val = sum(export_val)) %>%
  select(export_val, country, product)

products_oec <- read_xlsx("./data/product_oec.xlsx")$`HS4 ID`
products <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 6) %>%
  select(Code, Description, `Code Parent`) %>%
  rename(product = Code,
         parent_code = `Code Parent`,
         description = Description) %>%
  distinct(product, .keep_all = TRUE)
products
countries_2019 <- read_csv("./data/countries_2019_oec.csv")$Country

country_codes <- read_csv("./data/country_codes_V202102.csv",
                          col_types = "ccccc") %>%
  rename(country = country_name_full)

country_codes[country_codes$country %in% countries_2019, "country"]

trade_2019 <- read_csv("./data/BACI_HS92_Y2019_V202102.csv",
                       col_types = "dcccdd") %>%
  rename(year = t,
         product = k,
         importer = j,
         exporter = i,
         export_val = v,
         export_qua = q) %>%
  left_join(products)

taiwan <- read_csv("./data/exports-2019-taiwan.csv",
                   col_types = "ccccccccd") %>%
  select(`HS4 ID`, `Trade Value`) %>%
  rename(product = `HS4 ID`,
         export_val = `Trade Value`) %>%
  mutate(country = "1000",
         export_val = export_val/1000,
         product = substr(product, nchar(product) - 3, nchar(product))) %>%
  group_by(country, product) %>%
  summarise(export_val = sum(export_val)) %>%
  select(export_val, country, product) %>%
  filter(product %in% products_oec)

#### Testing ####
taiwan %>%
  group_by(country) %>%
  summarise(export_val = sum(export_val))

trade_2019 %>%
  filter(exporter == "598") %>%
  group_by(exporter) %>%
  summarise(export_val = sum(export_val))

trade_2019 %>%
  filter(exporter == "214", parent_code == "6305") %>%
  group_by(exporter) %>%
  summarise(export_val = sum(export_val))

trade_2019 %>%
  filter(exporter == "214", parent_code == "6105") %>%
  group_by(exporter) %>%
  summarise(export_val = sum(export_val))

trade_2019 %>%
  filter(importer == "214") %>%
  group_by(importer) %>%
  summarise(export_val = sum(export_val))

#### Exports Panel ####
exports_panel <- trade_2019  %>%
  select(export_val, exporter, parent_code) %>%
  rename(country_code = exporter,
         product = parent_code) %>%
  left_join(country_codes) %>%
  filter(iso_3digit_alpha %in% countries_oec$iso_3digit_alpha) %>%
  #       product %in% products_oec) %>%
  #filter(country_code != "490") %>%
  group_by(country_code, product) %>%
  summarise(export_val = sum(export_val)) %>%
  rename(country = country_code) %>%
  select(export_val, country, product)

exports_panel <- exports_panel %>%
  rbind(taiwan)

exports_panel %>%
  filter(product == "8113", country == "392") %>%
  summarise(export_val = sum(export_val))

exports_panel %>%
  filter(product == "6305", country == "DOM") %>%
  summarise(export_val = sum(export_val))

#### ECI ####

rca_panel <- computeRCA(exports_panel)
complexity_panel <- computeComplexity(rca_panel)
m_matrix <- computeM(complexity_panel)
M <- m_matrix
diversity_0 <- computeDiversity0(m_matrix)
ubiquity_0 <- computeUbiquity0(m_matrix)

M_tilde_ECI <- computeMTilde(m_matrix,
                             diversity_0,
                             ubiquity_0,
                             flag = "ECI")
eci <- computeRanks(M_tilde_ECI)
rank_eci <- data.frame(country = names(eci),
                       eci = -eci,
                       rank = rank(eci))

phi <- computeProximity(M, ubiquity_0)
d <- computeDistance(M, phi)
relatedness <- as.data.frame(d) %>%
  mutate(country = row.names(.)) %>%
  tidyr::gather(key = "product", value = "relatedness", -country)

M_tilde_PCI <- computeMTilde(m_matrix,
                             diversity_0,
                             ubiquity_0,
                             flag = "PCI")
pci <- computeRanks(M_tilde_PCI)
rank_pci <- data.frame(product = names(pci),
                       pci = -pci,
                       rank = rank(pci))

resumen <- left_join(rca_panel, rank_pci, by = "product") %>%
  left_join(exports_panel)

parent_products <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 4) %>%
  select(Code, Description) %>%
  rename(product = Code,
         description = Description) %>%
  distinct(product, .keep_all = TRUE)

rca_panel <- rca_panel %>%
  left_join(parent_products)

export_product <- exports_panel %>%
  group_by(product) %>%
  summarise(export_val = sum(export_val))

export_country <- exports_panel %>%
  group_by(country) %>%
  summarise(export_val = sum(export_val)) %>%
  left_join(country_codes, by = c("country" = "country_code"))

View(comparison_rca)
comparison_rca <- comparison_rca %>%
  mutate(comparison = rca.x == rca.y)
