setwd("C:/Users/fuent/OneDrive - The University of Chicago/Spring 2021/Data Visualization/Final Project/data")

source(file.path("..", "utils", "tidyinei.R"))

library(purrr)
library(haven)
library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)
library(lubridate)

# Defining years and modules of analysis

years <- 2004:2020 # years of analysis
modules <- c("02", "34")  # 02: Population; 34: income/expenditure summary

# Downloading income/expenditure summary and population modules for each year

for (i in 1:length(years)) {
  for (j in 1:length(modules)) {
    get_enaho(years[i], modules[j])
  }
}

# Combining datasets within years and then across years

pobreza <- data.frame()

for (k in 1:length(years)) {
  sumaria_file <- paste0("sumaria-", years[k], ".sav")
  mod200_file  <- paste0("enaho01-", years[k], "-200.sav")
  
  key_vars <- c("año", "conglome", "vivienda", "hogar")
  
  vars <- c(key_vars, "ubigeo", "dominio", "estrato", "mieperho",
            "totmieho", "gashog2d", "factor07", "pobreza", "ld")
  
  sumaria <-
    read_sav(sumaria_file) %>%
    zap_labels() %>%
    zap_formats() %>%
    rename_all(., tolower)
    
  if (sum(colnames(sumaria) == "ld") == 0)   # If variable was not present
    sumaria <- mutate(sumaria, ld = NA)
  
  sumaria <- 
    sumaria %>%
    select(vars) %>%
    mutate_all(., as.character()) %>%
    mutate(conglome = str_pad(str_squish(conglome), 6, "left", "0"),
           vivienda = str_pad(str_squish(vivienda), 3, "left", "0"),
           hogar = str_pad(str_squish(hogar), 2, "left", "0"))

  mod_200 <-
    read_sav(mod200_file) %>%
    zap_labels() %>%
    zap_formats() %>%
    rename_all(., tolower) %>%
    select(año, conglome, vivienda, hogar, facpob07, p203, p204) %>%
    mutate_all(., as.character()) %>%
    mutate(conglome = str_pad(str_squish(conglome), 6, "left", "0"),
           vivienda = str_pad(str_squish(vivienda), 3, "left", "0"),
           hogar = str_pad(str_squish(hogar), 2, "left", "0"))
    
  pobreza_temporal <-
    mod_200 %>%
    left_join(sumaria, by = key_vars)
  
  pobreza <- rbind(pobreza, pobreza_temporal)
  remove(sumaria, sumaria_file, mod_200, mod200_file, pobreza_temporal)
}

# Reviewing NAs presence

sapply(pobreza, function(x) sum(is.na(x))) # There is NAs for the spatial-deflactor (`ld`)

# `ld` variable imputation

ld_3_strat_layers <-
  pobreza %>%
  filter(!is.na(ld)) %>%
  group_by(ubigeo, dominio, estrato) %>%
  summarise(ld3 = mean(ld, na.rm = T))

ld_2_strat_layers <-
  pobreza %>%
  filter(!is.na(ld)) %>%
  group_by(dominio, estrato) %>%
  summarise(ld2 = mean(ld, na.rm = T))

pobreza <-
  pobreza %>%
  left_join(ld_3_strat_layers, by = c("ubigeo","dominio", "estrato")) %>%
  left_join(ld_2_strat_layers, by = c("dominio", "estrato")) %>%
  mutate(ld_final = ifelse(is.na(ld), ld3, ld)) %>%  # Only imputing NA's by the three-layer stratification
  mutate(ld_final = ifelse(is.na(ld_final), ld2, ld_final)) %>% # For those remaining, imputation is based on two-layer stratification
  mutate(ld_final = ifelse(as.numeric(ubigeo) %/% 100 == 1501, 1, ld_final)) %>% # Spatial-deflactor is 1 for Lima Metropolitana (1501XX), where XX defines districts
  select(-ld, -ld3, -ld2)

sapply(pobreza, function(x) sum(is.na(x))) # There is only NAs for p204. It's fine!

# Calculating the time-deflactor using the Central Bank of Peru website

cpi_url_base <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/api/PN01270PM/json/"
cpi_url_full <- paste0(cpi_url_base, first(years), "-1/", last(years), "-12", "/ing")
cpi_listed <- read_json(cpi_url_full)$periods
cpi_listed <- unname(unlist(cpi_listed))

# Tidying CPI dataset
cpi_data <- data.frame(month_year = rep(NA, length(cpi_listed) / 2),
                       cpi = rep(NA, length(cpi_listed) / 2))

for (l in 1:(length(cpi_listed) / 2)) {
  cpi_data$month_year[l] <- cpi_listed[1 + (l - 1) * 2]
  cpi_data$cpi[l] <- cpi_listed[2 + (l - 1) * 2]
}

cpi_data <-
  cpi_data %>%
  mutate(month_year  = lubridate::my(month_year),
         year = year(month_year)) %>%
  group_by(year) %>%
  summarise(cpi = mean(as.numeric(cpi))) %>%  # Average CPI by year
  ungroup() %>%
  mutate(deflactor = cpi/cpi[year == last(years)] ) %>%  # Time-deflactor based on the last year of analysis
  select(-cpi)

# Adding time-deflactor to main dataset
pobreza <-
  pobreza %>%
  mutate(año = as.numeric(año)) %>%
  left_join(cpi_data, by = c("año" = "year")) %>%
  mutate(real_expenditure = (gashog2d / ld_final) / deflactor)

# Recoding region units and poverty variable
pobreza <- pobreza %>%
  mutate(region = as.numeric(ubigeo) %/% 10000, # Two first digits of ubigeo depicts region
         region = ifelse(region == 15 & as.numeric(ubigeo) %/% 100 != 1501, 26, region), # Treating the rest provinces of Lima region as a different region
         region = ifelse(region == 7 | as.numeric(ubigeo) %/% 100 == 1501, 27, region), # Treating the capital city, Lima Metropolitana, as a region
         poverty = pobreza == 3, # pobreza is categorical variable--> 1: extreme poverty, 2: non-extreme poverty, 3: non-poverty
         poverty = (1 - poverty) * 100) # poverty is a dummy variable--> 1: poverty, 0: non-poverty 

# Calculating regional poverty across the time
regional_poverty <-
  pobreza %>%
  # Only considering those who belongs to the household (p204==1)
  # And excluding those who are domestic workers (p203==8) or only renting a room (p203==9) 
  filter(p204 == 1, !p203 %in% c(8, 9)) %>% 
  group_by(año, region) %>%
  summarise(poverty = weighted.mean(poverty, facpob07)) %>%
  ungroup()

# Calculating national poverty across the time
national_poverty <-
  pobreza %>%
  filter(p204 == 1, !p203 %in% c(8, 9)) %>% 
  group_by(año) %>%
  summarise(poverty = weighted.mean(poverty, facpob07)) %>%
  mutate(region = 0) %>%
  ungroup()

# Region of codes
region_code <- read_csv("region_code.csv")

# Binding regional and national poverty across the time
regional_poverty <-
  rbind(regional_poverty, national_poverty) %>%
  left_join(region_code, by = c("region" = "region_code")) %>%
  arrange(año, region) %>%
  pivot_wider(names_from = año, values_from  = poverty) # wide version

# Writing the csv file
write_excel_csv(regional_poverty, file.path("..", "regional_poverty_wide.csv"))