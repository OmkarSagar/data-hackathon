
library(pander)
library(tidyverse)
library(ggmap)
library(tree)
library(maptree)
library(ROCR)
library(modelr)
library(janitor)
library(dplyr)
library(stringr)
library(readxl)
library(reshape2)


names(adu_long)


# county with the highest per-capita adu construction
county_winner <- adu_long %>%
  group_by(county_fips) %>%
  mutate(total_const = sum(no_constructed),
         total_pop = sum(population),
         adus_const_per_cap = total_const / total_pop) %>%
  slice_max(adus_const_per_cap)


# CA rental rates by county, 2017 and 2020
rent17 <- read.csv("rent_housing_data/rent_2017.csv")
rent20 <- read.csv("rent_housing_data/rent_2020.csv")
names(rent17)[2:6] <- c('0','1','2','3','4')
names(rent20)[2:6] <- c('0','1','2','3','4')

cleanrent <- function(df) {
  df %>%
    clean_names() %>%
    dplyr::rename(studio=2, one=3, two=4, three=5, four=6) %>%
    mutate(county = stringr::str_replace(str_to_lower(county), "\\s", "_")) %>%
    mutate(county = gsub("_$", "", as.character(county))) %>%
    mutate(state = 'CA') %>%
    dplyr::rename_with(~gsub("^x", "", .x)) #%>%
    mutate(vars(2:6), as.numeric(gsub,"[\\$,]", ""))
}

rent17 <- rent17 %>% cleanrent()
rent20 <- rent20 %>% cleanrent()
rent17 %>% head() %>% pander()
rent20 %>% head() %>% pander()

# CA historical median rent estimates 2017-2020
median_rents <- read_csv("rent_housing_data/Apartment_List_Rent_Estimates_State_2022_4.csv")
median_ca <- median_rents %>%
  clean_names() %>%
  filter(state_name == "California") %>%
  dplyr::rename_with(~gsub("^x", "", .x)) %>%
  dplyr::rename(state = state_name) %>%
  mutate(state = replace(state, state == "California", "CA")) %>%
  mutate(bedroom_size = stringr::str_replace(str_to_lower(bedroom_size), "\\s", "_")) %>%
  mutate(bedroom_size = sub("^_", "", bedroom_size)) %>%
  mutate(bedroom_size = gsub("^_", "", bedroom_size)) %>%
  mutate(bedroom_size = gsub("^(\\d)(br)$", "\\1_\\2", bedroom_size))

View(median_ca)
median_ca %>% head() %>% pander()

# CA historical median rent estimates by county and city 2017-2020
median_rents2 <- read_csv("rent_housing_data/Apartment_List_Rent_Estimates_County_2022_5.csv")
median_rents3 <- read_csv("rent_housing_data/Apartment_List_Rent_Estimates_City_2022_5.csv")

# fips codes: 6037, 6059, 6071 resp.
median_county <- median_rents2 %>%
  clean_names() %>%
  filter(grepl("(CA)$", county_name)) %>%
  dplyr::rename_with(~gsub("^x", "", .x)) %>%
  dplyr::rename(county = county_name) %>%
  dplyr::rename(county_fips = fips_code) %>%
  mutate(county = gsub(" County, CA", "", county)) %>%
  mutate(county = stringr::str_replace(str_to_lower(county), "\\s", "_")) %>%
  mutate(bedroom_size = stringr::str_replace(str_to_lower(bedroom_size), "\\s", "_")) %>%
  mutate(bedroom_size = gsub("^_", "", bedroom_size)) %>%
  mutate(bedroom_size = gsub("^(\\d)(br)$", "\\1_\\2", bedroom_size)) %>%
  filter(county == "los_angeles" | county == "orange" | county == "san_bernardino")

# can't tell what county from the city's geoid (NOT a fips code technically)
median_city <- median_rents3 %>%
  clean_names() %>%
  filter(grepl("(CA)$", city_name)) %>%
  dplyr::rename_with(~gsub("^x", "", .x)) %>%
  dplyr::rename(city = city_name) %>%
  mutate(city = gsub(", CA", "", city)) %>%
  mutate(city = stringr::str_replace(str_to_lower(city), "\\s", "_")) %>%
  mutate(bedroom_size = stringr::str_replace(str_to_lower(bedroom_size), "\\s", "_")) %>%
  mutate(bedroom_size = gsub("^_", "", bedroom_size)) %>%
  mutate(bedroom_size = gsub("^(\\d)(br)$", "\\1_\\2", bedroom_size))

median_county %>% head()
median_city %>% head()

df <- split(median_county, (1:nrow(median_county) - 1) %/%3 )
prices <- as.data.frame(t(median_county[c(5:69)]))
vars(prices) <- median_county$bedroom_size
prices$month <- rownames(prices)
prices <- melt(prices, id.vars=c("month"))
prices

median_county %>%
  na.omit() %>%
  mutate(time = names(median_county[-c(1:4)])) %>%
  gather(key = 'bedroom_size', value = 'time')
ggplot(aes(x=as.number(month), y=prices, group = variable, color = variable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~id) +
  theme(legend.position = "none") +
  labs(y="Monthly Rent Estimates", x = "Time")

# CA historical median prices of existing singly-family detached homes by county 2017-2020
# Median Price (existing single-family detached homes only)
# *Note	Sales for California are seasonally adjusted and annualized
# 	LA Metro is a 5-county region that includes Los Angeles County, Orange County, Riverside County, San Bernardino County, and Ventura County
# 	Inland Empire includes Riverside County and San Bernardino County
# 	S.F. Bay Area has been redefined to include the following counties: Alameda, Contra Costa, Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma
median_housing <- read_excel("rent_housing_data/housing_prices_by_county.xls")
median_housing %>% head() %>% pander()

