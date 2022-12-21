library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(janitor)
library(usmap)
library(maps)
library(mapdata)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
hrbrthemes::import_roboto_condensed()

adu = readxl::read_xls("ADU.xls")
view(adu)
adu %>%
  colnames()


# Changes raw ADU data file to usable and shortened strings
adu_num = adu %>%
  select(-letter_grade, -yea_adopted_or_amended) %>%
  mutate(no_permits = no_of_ADUs_permitted_2018_2020) %>%  ifelse(no_of_ADUs_permitted_2018_2020 > 0, log(no_of_ADUs_permitted_2018_2020),0)) %>%
  mutate(no_constructed = no_of_ADUs__constructed_2018_2020) %>% ifelse(no_of_ADUs__constructed_2018_2020 > 0, log(no_of_ADUs__constructed_2018_2020),0)) %>%
  select(-no_of_ADUs_permitted_2018_2020, -no_of_ADUs__constructed_2018_2020) %>%
  mutate(jurisdiction = stringr::str_replace(str_to_lower(jurisdiction), "\\s", "_")) %>%
  as_tibble()

view(adu_num)


# Imports FIPS information file
CAL_LAT_and_LONG <- readxl::read_xls("CAL LAT and LONG.xls")
CAL_LAT_and_LONG %>%
  colnames()


# Cleans names
clal <- CAL_LAT_and_LONG %>%
  mutate(city_ascii = stringr::str_replace(str_to_lower(city_ascii), "\\s", "_")) %>%
  as_tibble() %>%
  mutate(jurisdiction = city_ascii)
#view(clal)



# Joins ADU dataframe and fips / geo information, removes unnecessary geo data
adu_geo_data = adu_num %>%
  left_join(clal, by = "jurisdiction")

view(adu_geo_data)

adu_geo_data %>%
  colnames()



# Sorts from most northern to most southern areas by longitude
adu_long = adu_geo_data %>%
  arrange(lng, decreasing = TRUE) %>%
  filter(no_permits !=0 | no_constructed !=0)


# Names the vector including the fips code
GEOID = adu_long$county_fips

view(adu_long)


# renames the fips column in 2 ways so it is usable with different standards of USMAPS and GGPlot2
adu_const_no_na = adu_long %>%
  mutate(GEOID = county_fips) %>%
  mutate(fips = GEOID)

head(adu_const_no_na)

# converts the geoid to strings for use in one of the map packages
adu_const_no_na$GEOID <- as.character(adu_const_no_na$GEOID) %>%
  filter(!is.na(.$no_constructed)) %>%
  class(GEOID) <- "Character"
  mutate(GEOID = as.character(adu_long$GEOID))

head(adu_const_no_na)

# Removes LA from the map, because LA has 10x the number of new ADU's as compared to the next highest area.
# Need to figure out a way to display both eventually, maybe LA as an exemplor of the ADU boom
fips_groups = adu_const_no_na %>%
  filter(jurisdiction != "los_angeles") %>%
  aggregate(no_constructed ~ fips, data=., FUN=sum) %>%
  as_tibble()
aggregate(no_constructed~county_fips, ., FUN=c)
mutate()
group_by(county_fips)

head(fips_groups)

without_la = adu_const_no_na %>%
  filter(jurisdiction != "los_angeles")

head(without_la)

# dataframe that calculates number of ADUs per capita
fips_n_const = adu_const_no_na %>%
  mutate(const_per_capita = (no_constructed/population))

# Map of per capita ADU construction
usmap::plot_usmap(data = fips_n_const, include="CA", values = "const_per_capita", name = "New ADU Construction per capita", labels=TRUE) +
  scale_fill_continuous( low = "yellow", high = "red",
                         name = "New ADU Construction per capita", label = scales::comma
  ) +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Where new ADUs are being built"), caption = "Source: @littlemissdata")

# Map of total ADU construction without LA
usmap::plot_usmap(data = fips_groups, include="CA", values = "no_constructed", name = "New ADU Construction", labels=TRUE) +
  scale_fill_continuous( low = "yellow", high = "red",
                         name = "New ADU Construction", label = scales::comma
  ) +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Where new ADUs are being built"), caption = "Source: @littlemissdata")


# ADU + GEO + Population CSV File
write.csv(adu_long, file = "adu_master.csv",
          row.names = FALSE)

view(adu_long)


# Importing 2017 rent prices
rent_2017 <- read_csv("rent_data/rent_2017.csv")
rent_2017 = janitor::clean_names(rent_2017)
rent_2017$state <- 'CA'
# Map of rental prices by county in 2017
plot_usmap(data = rent_2017, include="CA", values = "x4_br", name = "2017 Rent Distribution", labels=TRUE)
scale_fill_continuous( low = "yellow", high = "red",
name = "New ADU Construction", label = scales::comma)

# Reading cali demographic file
cali <- read_csv('cali_demographics.csv')
view(cali)
cali <- cali %>%
  select(-median_household_income_dollars_estimate)

cali <- cali %>%
  rename(
    city = geography
  )
# Importing master csv file
master_csv <- read_csv('adu_master.csv')
master_csv <- merge(master_csv, cali, by='city', all.x=TRUE)
