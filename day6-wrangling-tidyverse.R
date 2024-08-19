rm(list = ls()) # clear global environment

library(tidyverse)
library(here)
library(janitor)

# read in data ----
# here is just establishing a file path using the current working directory
wb_indicators <- read_csv(here("data", "wb_indicators.csv"), na = c("..", ""))
wb_metadata <- read_csv(here("data", "wb_indicators_metadata.csv"))

# tidying data ----

wb_indicators_long <- wb_indicators %>%
  pivot_longer(cols = "2001 [YR2001]":"2020 [YR2020]",
               names_to = "year",
               values_to = "indicator_value")

# getting rid of the year_chr column we just created with the separate function, as well as the country and series
# code because they're not essential
wb_data_clean <- wb_indicators_long %>%
  tidyr::separate(col = year, into = c("year", "year_chr"), sep = " ") %>%
  dplyr::select(-year_chr, -"Country Code", -"Series Code")

# each of the indicator names is now a column name
wb_data_tidy <- wb_data_clean %>%
  tidyr::drop_na("Series Name") %>%
  tidyr::pivot_wider(names_from = "Series Name",
                     values_from = indicator_value)

# cleaning up the column names ----
names(wb_data_tidy) <- c("country", "year", "access_clean_fuels_pp", "access_electricity_pp", "co2_emissions_kt",
                         "fossil_fuel_cons_pt", "water_stress")

# excluding rows based on conditions ----
us_wb <- wb_data_tidy %>%
  dplyr::filter(country == "United States") # only data for US

nicaragua_co2 <- wb_data_tidy %>%
  dplyr::filter(country == "Nicaragua") %>%
  select(year, co2_emissions_kt) # only co2 emission data for Nicaragua and the year

wb_subset <- wb_data_tidy %>%
  select(-c(water_stress, access_electricity_pp)) # subset without certain columns

wb_new_names <- wb_data_tidy %>%
  rename(elec = access_electricity_pp, co2 = co2_emissions_kt) # renaming some columns

# changing data types ----
# wb_data_tidy$year <- as.numeric(wb_data_tidy$year) # this does the same thing as the mutate function, but its
# just from base r functions. Using piping in dplyr is a little more straight forward / cleaner

wb_data_tidy <- wb_data_tidy %>%
  mutate(year = as.numeric(year))

class(wb_data_tidy$year)

# unit conversions ----
wb_co2_tons <- wb_data_tidy %>%
  mutate(co2_tons = co2_emissions_kt*1000) # added a new column to show co2 emissions in tons rather than kilatons
# (this doesn't get rid of the kilatons column, it just adds a new one)

# using group_by() and summarize() ----
co2_total <- wb_data_tidy %>%
  group_by(country) %>%
  summarize(total_co2_kt = sum(co2_emissions_kt, na.rm=TRUE))
