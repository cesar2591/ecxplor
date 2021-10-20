rm(list = ls())
options(scipen = 999)

library(tidyr)
library(dplyr)
library(readr)
library(economiccomplexity)

world_trade2 <- read_csv("./data/WtoData_20211018200240.csv") %>%
  filter(`Indicator Code` == "ITS_MTV_AX") %>% # Annual exports ITS_MTV_AX
  rename(country = `Reporting Economy ISO3A Code`,
         product = `Product/Sector Code`,
         value = Value) %>%
  group_by(country, product) %>%
  summarize(value = mean(value)) %>%
  na.omit() %>%
  select(value, country, product)

bi <- balassa_index(world_trade2)
bi[1:5, 1:5]
com_fit <- complexity_measures(bi, method = "eigenvalues")
com_fit$complexity_index_product
com_fit$complexity_index_country
pro <- proximity(bi)
pro$proximity_country[1:5,1:5]
pro$proximity_product[1:5,1:5]

co <- complexity_outlook(
  economiccomplexity_output$balassa_index,
  economiccomplexity_output$proximity$proximity_product,
  economiccomplexity_output$complexity_measures$complexity_index_product
)
co$complexity_outlook_index[1:5]
