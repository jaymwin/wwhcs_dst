
library(tidyverse)
library(sf)
library(readxl)
library(writexl)

select <- dplyr::select

source(here::here('99-source_functions.R'))

# ebird
final_product_ebird <-
  read_rds(here::here('outputs/priority_watersheds_ebird.rds')) %>%
  select(
    `Priority Level - eBird (Aggregate)` = priority_watershed, 
    `Priority Level - eBird (Conservation Capital 0-3)` = cc_rank
  )

sites <- 
  read_excel(here::here('data/sedinger_study_sites.xlsx')) %>%
  select(1:4) %>%
  st_as_sf(coords = c('Easting', 'Northing'), crs = 4326)

ebird_sites <- 
  st_join(sites, final_product_ebird, join = st_within) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(1, 6, 5, 2:4)
glimpse(ebird_sites)

ebird_sites %>%
  write_xlsx(here::here('sedinger_sites_ebird_ranks.xlsx'))
