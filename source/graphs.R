rm(list = ls())
options(scipen = 999)

library(ggplot2)
library(readr)
library(readxl)
library(dplyr)

#### PIC vs ECI ####
country_codes <- read_csv("./data/country_codes_V202102.csv",
                          col_types = cols(.default = col_character()))

eci_ranking <- read_csv("./data/Country Complexity Rankings 1995 - 2019.csv") %>%
  select(1:2)

eci_ranking[eci_ranking$Country == "Côte d'Ivoire", "Country"] = "Cote d'Ivoire"

region <- read_csv("./data/Metadata_Country_API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_3161026.csv") %>%
  select(`Country Code`, Region) %>%
  rename(country_code = `Country Code`,
         region = Region) %>%
  na.omit()

gdp_pc <- read_csv("./data/API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_3161026.csv",
                   skip = 4,
                   col_types = cols_only(`Country Name` = col_character(),
                                         `Country Code` = col_character(),
                                         `2019` = col_guess()))

pop <- read_csv("./data/API_SP.POP.TOTL_DS2_en_csv_v2_3158886.csv",
                skip = 4,
                col_types = cols_only(`Country Name` = col_character(),
                                      `Country Code` = col_character(),
                                      `2019` = col_guess()))

data <- eci_ranking %>%
  left_join(country_codes, by = "Country") %>%
  left_join(gdp_pc, by = c("iso_3digit_alpha" = "Country Code")) %>%
  left_join(pop, by = c("iso_3digit_alpha" = "Country Code")) %>%
  select(iso_3digit_alpha, Country, `ECI Rank 2019`, `2019.x`, `2019.y`) %>%
  rename(gdp_pc = `2019.x`,
         pop = `2019.y`,
         country_code = iso_3digit_alpha,
         country_name = Country,
         eci = `ECI Rank 2019`) %>%
  filter(!is.na(gdp_pc),
         !is.na(pop)) %>%
  mutate(eci_rerank = rank(eci))

ggplot(data, aes(x = eci_rerank, y = gdp_pc, size = pop)) +
  geom_point(alpha = 0.3) +
  theme_minimal()
#### Ratio Exportaciones/PIB ####
products <- read_xlsx("./data/UN Comtrade Commodity Classifications.xlsx") %>%
  filter(nchar(Code) == 6) %>%
  select(Code, Description, `Code Parent`) %>%
  rename(product = Code,
         parent_code = `Code Parent`,
         description = Description) %>%
  distinct(product, .keep_all = TRUE)

export <- read_csv("./data/BACI_HS92_Y2019_V202102.csv",
                   col_types = "dcccdd") %>%
  rename(product = k,
         year = t,
         exporter = i,
         importer = j,
         value = v,
         quantity = q) %>%
  left_join(products,
            by = "product") %>%
  left_join(select(country_codes, country_code, iso_3digit_alpha),
            by = c("importer" = "country_code")) %>%
  rename(importer_code = iso_3digit_alpha) %>%
  left_join(select(country_codes, country_code, iso_3digit_alpha),
            by = c("exporter" = "country_code")) %>%
  rename(exporter_code = iso_3digit_alpha)

export %>%
  filter(exporter_code == "HTI") %>%
  group_by(importer_code) %>%
  summarise(value = sum(value)) %>%
  arrange(-value) %>%
  View()

export %>%
  filter(exporter_code == "HTI") %>%
  summarise(value = sum(value))
