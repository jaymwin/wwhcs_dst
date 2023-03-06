

# load packages/functions -------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(raster)
library(tmap)
library(exactextractr)
library(patchwork)
library(mapview)
library(wesanderson)

select <- dplyr::select
theme_set(theme_minimal())

source(here::here('99-source_functions.R'))


# read in huc, EL shapefiles ----------------------------------------------

# watersheds
huc_watersheds <- 
  read_sf(
  here::here('data/dst_spatial.gpkg'), 
  layer = 'huc_watersheds'
  )
  
# wi border too
wi_border <- 
  read_sf(
  here::here('data/dst_spatial.gpkg'), 
  layer = 'wi_border'
)


# weighted sum - CC raster ------------------------------------------------

# find CC inputs which have already been through fuzzy membership
cc_inputs <- 
  dir_ls(here::here('data/ebird_inputs/ebird_cc_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'hunter|ecological|distribution_fuzzy'))

# pull paths
cc_inputs_paths <- 
  cc_inputs %>%
  pull(value)

# stack raster layers and plot
cc_inputs_stack <- 
  cc_inputs_paths %>% 
  stack()
plot(cc_inputs_stack)

# define weights (these follow the order of rasters in the stack)
cc_weights <- 
  cc_inputs %>%
  mutate(wt = c(0.44, 0.13, 0.14, 0.10, 0.19))

# perform weighted sum and plot
cc_layer <- 
  weighted_sum(
  stack = cc_inputs_stack,
  w = cc_weights$wt
)

cc_layer_rescaled <- calc(cc_layer, fun = rescale_01)
plot(cc_layer_rescaled)


# weighted sum - CO raster ------------------------------------------------

co_inputs <- 
  dir_ls(here::here('data/ebird_inputs/ebird_co_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'ecological|resource|fuzzy'))

co_inputs_paths <- 
  co_inputs %>%
  pull(value)

co_inputs_stack <- 
  co_inputs_paths %>% 
  stack()

co_weights <- 
  co_inputs %>%
  mutate(wt = c(0.48, 0.19, 0.33))

co_layer <- 
  weighted_sum(
  stack = co_inputs_stack,
  w = co_weights$wt
)

co_layer_rescaled <- calc(co_layer, fun = rescale_01)
plot(co_layer_rescaled)


# huc 12s - CC ---------------------------------------------------

# Calculate vector of mean CC for each watershed
huc_watersheds$conservation_capital <- exact_extract(cc_layer_rescaled, huc_watersheds, 'mean')

# plot cc ranks by watershed
ggplot() +
  geom_sf(data = huc_watersheds, aes(fill = conservation_capital)) +
  scale_fill_viridis_c()

ggplot() +
  geom_histogram(data = huc_watersheds, aes(x = conservation_capital))

# turn into quantiles
huc_watersheds <- 
  huc_watersheds %>%
  # group_by(ecological_landscape) %>%
  # subtract one from quantile rank because it starts at 1 using ntile() function
  # needs to start at 0 to follow the DST
  mutate(
    cc_1_6 = ntile(conservation_capital, 6),
    cc_1_4 = ntile(conservation_capital, 4),
    cc_1_3 = ntile(conservation_capital, 3)
    )

# plot it
huc_cc_rank <- 
  huc_watersheds %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(cc_1_6)), color = 'grey', size = 0.1) +
  scale_fill_viridis_d(name = 'Statewide rank') +
  guides(fill = 'none') +
  ggtitle('CC') +
  theme(axis.text = element_blank())

huc_watersheds %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(cc_1_3)), color = 'grey', size = 0.1) +
  scale_fill_viridis_d(name = 'Statewide rank') +
  guides(fill = 'none') +
  ggtitle('CC') +
  theme(axis.text = element_blank())

huc_watersheds %>%
  mutate(
    cc_16_13 = case_when(
      cc_1_6 <= 2 ~ 1,
      cc_1_6 >= 5 ~ 3,
      TRUE ~ 2
    )
  ) %>%
  mutate(diff = cc_16_13 - cc_1_3) %>%
  ggplot() +
  geom_histogram(aes(diff))

huc_watersheds %>%
  mutate(
    cc_16_13 = case_when(
      cc_1_6 <= 2 ~ 1,
      cc_1_6 >= 5 ~ 3,
      TRUE ~ 2
    )
  ) %>%
  mutate(diff = cc_16_13 - cc_1_3) %>%
  filter(diff != 0)


# huc 12s - CO ------------------------------------------------------------

# Calculate vector of mean CO for each watershed
huc_watersheds$conservation_opportunity <- exact_extract(co_layer_rescaled, huc_watersheds, 'mean')

# plot co ranks by watershed
ggplot() +
  geom_sf(data = huc_watersheds, aes(fill = conservation_opportunity)) +
  scale_fill_viridis_c()

ggplot() +
  geom_histogram(data = huc_watersheds, aes(x = conservation_opportunity))

# turn into quantiles
huc_watersheds <- 
  huc_watersheds %>%
  # group_by(ecological_landscape) %>%
  mutate(
    co_1_6 = ntile(conservation_opportunity, 6),
    co_1_4 = ntile(conservation_opportunity, 4),
    co_1_3 = ntile(conservation_opportunity, 3)
  )

huc_co_rank <- 
  huc_watersheds %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(co_1_4)), color = 'grey', size = 0.1) +
  scale_fill_viridis_d(name = 'Rank') +
  ggtitle('CO') +
  theme(axis.text = element_blank())
huc_co_rank

huc_cc_rank + huc_co_rank
ggsave(here::here('figures/statewide_cc_co_watersheds.png'), width = 5, height = 3, units = 'in')

# save for Jason
huc_watersheds %>%
  st_write(
    # overwrite
    delete_dsn = TRUE,
    here::here('data/layers_for_jason/statewide_cc_co_ranks.shp')
    )

print('script 05 finished')
