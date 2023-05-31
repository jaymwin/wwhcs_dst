

# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ebirdst)
library(fs)

# housekeeping, load source functions
select <- dplyr::select
theme_set(theme_light() + theme(panel.grid.minor = element_blank()))
source(here::here('99-source_functions.R'))

# 70oq3a5ldvjq
# need to set api key to access data
# set_ebirdst_access_key("9l0a8eptu9ar", overwrite = TRUE)

# check ebirdst version
packageVersion('ebirdst')

# had to update ebirdst version to access 2021 data
# remove.packages('ebirdst')
# install.packages('ebirdst')


# download status and trends data -----------------------------------------

# create a dataframe of focal species (plus canvasback)
spp <- 
  tibble(
  # canvasback of interest to Jason Fleener for different project
  species = c('buwtea', 'canvas', 'mallar3', 'rinduc', 'wooduc')
)

# ebirdst data get saved here
data_dir <- ebirdst_data_dir()
# dir_ls(ebirdst_data_dir(), recurse = TRUE)
# fs::dir_delete(ebirdst_data_dir())

# only download ebird data if it doesn't already exist
if (!dir.exists(data_dir)) {

  # download for each species
  spp$species %>%
    walk(bulk_download)

  # check this worked
  dir_ls(ebirdst_data_dir(), recurse = TRUE)

}


# create season dates to filter ebird raster stacks -----------------------

# set up start and end dates
species_dates <- 
  tribble(
  # ebird code, 4-letter code, etc.
  ~focal_spp, ~spp_code, ~season, ~start_date, ~end_date,
  # wood duck
  'wooduc', 'wodu', 'breeding', '2021-04-15', '2021-08-15',
  'wooduc', 'wodu', 'fall', '2021-09-01', '2021-12-15',
  'wooduc', 'wodu', 'spring', '2021-02-01', '2021-03-31',
  # ring-necked duck
  'rinduc', 'rndu', 'breeding', '2021-05-15', '2021-08-15',
  'rinduc', 'rndu', 'fall', '2021-09-01', '2021-12-15',
  'rinduc', 'rndu', 'spring', '2021-02-01', '2021-04-30',
  # blue-winged teal
  'buwtea', 'bwte', 'breeding', '2021-05-01', '2021-08-15',
  'buwtea', 'bwte', 'fall', '2021-09-01', '2021-12-15',
  'buwtea', 'bwte', 'spring', '2021-02-01', '2021-04-15',
  # mallard
  'mallar3', 'mall', 'breeding', '2021-04-15', '2021-08-15',
  'mallar3', 'mall', 'fall', '2021-09-01', '2021-12-15',
  'mallar3', 'mall', 'spring', '2021-02-01', '2021-03-31'
)

# save dates
species_dates %>% 
  write_csv(here::here('outputs/species_dates_table.csv'))

# now filter down to species/seasons we need
species <- c('buwtea', 'mallar3', 'rinduc', 'wooduc')
season <- c('breeding', 'fall', 'spring')

# find ebird data paths for each species
sp_path_df <- 
  dir_ls(ebirdst_data_dir(), recurse = TRUE) %>%
  as_tibble() %>%
  mutate(spp = str_extract(value, str_flatten(species, '|'))) %>%
  drop_na() %>%
  group_by(spp) %>%
  # file path
  slice(1)
sp_path_df


# now iterate through ebird species/season -----------------------

# need WI border to crop ebird rasters
# this shapefile is from DNR geospatial data portal
wi <- read_sf(here::here('data/dst_spatial.gpkg'), 'wi_border')

# loop through each species, then season
# filter raster stack to dates, crop to wisconsin border, average encounter rate/abundance
# save raster
for (i in seq_along(species)) {
  
  # print(species[i])
  
  # filter season dates to focal species
  df_species <- species_dates %>%
    filter(focal_spp == species[i])
  
  # filter ebird data path to focal species
  sp_path <- sp_path_df %>%
    filter(spp == species[i]) %>%
    pull(value)

  print(sp_path)
  
  # now loop through each season
  for (j in seq_along(season)) {
    
    print(paste0(species[i], ', ', season[j]))
    
    # load occurrence raster; really just for transforming WI border and getting raster dates
    occ <- load_raster("occurrence", path = sp_path, resolution = "hr")

    # transform wisconsin to ebird raster projection
    wi_transformed <- wi %>%
      st_transform(., st_crs(occ))
    
    # filter species dates to focal season
    df_species_season <- df_species %>%
      filter(season == season[j])
    
    # extract start/end dates
    min_date <- df_species_season$start_date
    max_date <- df_species_season$end_date
    
    # grab 4-letter species code
    code <- df_species_season$spp_code

    # now figure out which weeks of the raster to use to match start/end dates
    date_vector <- parse_raster_dates(occ) %>%
      as_tibble() %>%
      mutate(week = row_number()) %>%
      select(week, date = value)
    
    # pull out these weeks
    season_weeks <- date_vector %>%
      filter(date >= min_date & date <= max_date) %>%
      pull(week)
    
    # now do the same for abundance
    abd <- load_raster("abundance", path = sp_path, resolution = "hr")

    # subset abundance stack to same dates
    abd_season <- abd[[min(season_weeks):max(season_weeks)]]

    # crop to wisconsin border
    abd_season <- crop(abd_season, wi_transformed) %>% # crop by WI border
      raster::mask(., wi_transformed) # %>%
      # raster::trim()
    
    # calculate average across weeks
    abd_season <- calc(abd_season, fun = mean, na.rm = T)
    
    # give raster species code
    names(abd_season) <- code
    plot(abd_season, main = str_c(code, ' ', season[j]))
    
    # save species x season raster for abundance
    abd_season %>%
      writeRaster(
        .,
        str_c(here::here('data/rasters_from_ebirdst'), '/', code, '_', season[j], '_abundance.tif'),
        overwrite = TRUE
      )
    
  }
  
}


# did this work? ----------------------------------------------------------

# read in all bwte layers, stack, plot - yes
dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'bwte') %>%
  as_tibble() %>%
  filter(str_detect(value, 'abundance.tif')) %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack() %>%
  plot()

# read in all mall layers, stack, plot - yes
dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'mall') %>%
  as_tibble() %>%
  filter(str_detect(value, 'abundance.tif')) %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack() %>%
  plot()

# read in all rndu layers, stack, plot - yes
dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'rndu') %>%
  as_tibble() %>%
  filter(str_detect(value, 'abundance.tif')) %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack() %>%
  plot()

# read in all wodu layers, stack, plot - yes
dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'wodu') %>%
  as_tibble() %>%
  filter(str_detect(value, 'abundance.tif')) %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack() %>%
  plot()


print('script 01 finished')

# next script, prepare the ebird rasters for use in the DST...
