

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
library(stars)

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
  
# ELs
eco_landscapes <- 
  read_sf(
  here::here('data/dst_spatial.gpkg'), 
  layer = 'eco_landscapes'
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
  dir_ls(here::here('data/expert_inputs/expert_cc_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'hunter|ecological|expert'))

# pull paths
cc_inputs_paths <- 
  cc_inputs %>%
  pull(value)

# stack raster layers and plot
cc_inputs_stack <- 
  cc_inputs_paths %>% 
  stack()
plot(cc_inputs_stack)

range(raster::values(cc_inputs_stack[[1]]), na.rm = TRUE)
range(raster::values(cc_inputs_stack[[2]]), na.rm = TRUE)
range(raster::values(cc_inputs_stack[[3]]), na.rm = TRUE)
range(raster::values(cc_inputs_stack[[4]]), na.rm = TRUE)
range(raster::values(cc_inputs_stack[[5]]), na.rm = TRUE)

# save plot here as well
tm <- tm_shape(cc_inputs_stack) +
  tm_raster(style = 'cont', palette = 'viridis', title = 'Value', legend.reverse = TRUE)
tm
# tmap_save(tm, here::here('figures/cc_inputs_expert.png'), width = 5000, height = 3000, dpi = 1000)

bio_layers <- stack(c(cc_inputs_stack[[2]], cc_inputs_stack[[1]], cc_inputs_stack[[5]]))
names(bio_layers) <- c('Breeding', 'Fall', 'Spring')
plot(bio_layers)

bio_layers_stars <- bio_layers %>%
  st_as_stars()

ggplot() +
  geom_stars(data = bio_layers_stars) +
  geom_sf(data = wi_border %>% st_transform(st_crs(bio_layers)), fill = NA) +
  facet_wrap(~band) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank()) +
  scale_fill_viridis_c(name = 'Value', na.value = NA, option = 'magma')
ggsave(here::here('figures/expert_cc_inputs.png'), height = 3, width = 6, units = 'in', dpi = 600)

# define weights (these follow the order of rasters in the stack)
cc_weights <- 
  cc_inputs %>%
  mutate(
    wt = c(
      0.14, # fall
      0.44, # breeding
      0.13, # eco services
      0.10, # hunter
      0.19 # spring
    )
  )

# perform weighted sum and plot
cc_layer <- 
  weighted_sum(
  stack = cc_inputs_stack,
  w = cc_weights$wt
)
plot(cc_layer)
 
tm_shape(cc_layer) +
  tm_raster(style = 'cont', palette = 'viridis', title = 'Value', legend.reverse = TRUE)

# save (then re-scale with fuzzy membership in arcgis pro)
cc_layer_rescaled <- calc(cc_layer, fun = rescale_01)
plot(cc_layer_rescaled)

cc_layer_rescaled %>%
  writeRaster(
    here::here('data/expert_outputs/expert_cc_layer_fuzzy.tif'), 
    overwrite = TRUE
    )

# # save to geopackage too
# cc_layer_rescaled %>% 
#   st_as_stars %>% 
#   write_stars(
#     here::here("outputs/dst_rasters.gpkg"),
#     driver = "GPKG",
#     options = c(
#       "RASTER_TABLE=expert_cc_layer_rescaled",
#       "APPEND_SUBDATASET=YES"
#     )
#   )

tm1 <- tm_shape(cc_layer_rescaled) +
  tm_raster(style = 'cont', palette = 'inferno', title = 'Value', legend.reverse = TRUE) +
  tm_layout(legend.position = c("right", "top"), main.title = 'CC')
tm1


# weighted sum - CO raster ------------------------------------------------

co_inputs <- 
  dir_ls(here::here('data/expert_inputs/expert_co_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'ecological|resource|core'))

co_inputs_paths <- 
  co_inputs %>%
  pull(value)

co_inputs_stack <- 
  co_inputs_paths %>% 
  stack()
plot(co_inputs_stack)

tm <- tm_shape(co_inputs_stack) +
  tm_raster(style = 'cont', palette = 'viridis', title = 'Value', legend.reverse = TRUE)
tm
# tmap_save(tm, here::here('figures/co_inputs_expert.png'), width = 5000, height = 3000, dpi = 1000)

co_weights <- 
  co_inputs %>%
  mutate(
    wt = c(
      0.48, # core breeding
      0.19, # eco services
      0.33  # resource user
    )
  )

co_layer <- 
  weighted_sum(
  stack = co_inputs_stack,
  w = co_weights$wt
)
plot(co_layer)

co_layer_rescaled <- calc(co_layer, fun = rescale_01)
plot(co_layer_rescaled)

# save (then re-scale with fuzzy membership in arcgis pro)
co_layer_rescaled %>%
  writeRaster(
    here::here('data/expert_outputs/expert_co_layer_fuzzy.tif'), 
    overwrite = TRUE
  )

# # save to geopackage too
# co_layer_rescaled %>% 
#   st_as_stars %>% 
#   write_stars(
#     here::here("outputs/dst_rasters.gpkg"),
#     driver = "GPKG",
#     options = c(
#       "RASTER_TABLE=expert_co_layer_rescaled",
#       "APPEND_SUBDATASET=YES"
#     )
#   )

tm1 <- tm_shape(co_layer_rescaled) +
  tm_raster(style = 'cont', palette = 'inferno', title = 'Value', legend.reverse = TRUE) +
  tm_layout(legend.position = c("right", "top"), main.title = 'co')
tm1


# ecological landscapes - CC ---------------------------------------------------

# For each of 17 ELs, we summed
# values of the CC raster model using zonal statistics and
# then calculated an average value for each EL

# Calculate vector of mean CC for each ecological landscape
eco_landscapes$conservation_capital <- 
  exact_extract(
    cc_layer_rescaled, 
    eco_landscapes %>% st_transform(., st_crs(cc_layer_rescaled)), 
    'mean'
    )

ggplot() +
  geom_sf(data = eco_landscapes, aes(fill = conservation_capital)) +
  scale_fill_viridis_c()

# turn into quantiles
eco_landscapes <- 
  eco_landscapes %>%
  mutate(cc_rank = ntile(conservation_capital, 5))

eco_landscapes %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(cc_rank))) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c('dodgerblue', 'honeydew3', 'khaki1', 'orange', 'red'), name = 'CC rank') +
  ggtitle('Conservation capital by ecological landscape - expert')


# ecological landscapes - CO ----------------------------------------------

# Calculate vector of mean CO for each ecological landscape
eco_landscapes$conservation_opportunity <- 
  exact_extract(
  co_layer_rescaled, 
  eco_landscapes %>% st_transform(., st_crs(co_layer_rescaled)), 
  'mean'
  )

ggplot() +
  geom_sf(data = eco_landscapes, aes(fill = conservation_opportunity)) +
  scale_fill_viridis_c()

# turn into quantiles
eco_landscapes <- 
  eco_landscapes %>%
  mutate(co_rank = ntile(conservation_opportunity, 5))

eco_landscapes %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(co_rank))) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c('dodgerblue', 'honeydew3', 'khaki1', 'orange', 'red'), name = 'CO rank') +
  ggtitle('Conservation opportunity by ecological landscape - expert')


# huc 12s - CC ---------------------------------------------------

# Calculate vector of mean CC for each watershed
huc_watersheds$conservation_capital <- 
  exact_extract(
    cc_layer_rescaled, 
    huc_watersheds %>% st_transform(., st_crs(cc_layer_rescaled)), 
    'mean'
  )

# no NAs
huc_watersheds %>%
  filter(is.na(conservation_capital))

# na_cc_watersheds <- 
#   huc_watersheds %>%
#   filter(is.na(conservation_capital)) %>%
#   pull(huc_id)

# plot cc ranks by watershed
ggplot() +
  geom_sf(data = huc_watersheds, aes(fill = conservation_capital)) +
  scale_fill_viridis_c()

# view interactively
huc_watersheds %>%
  mapview(zcol = 'conservation_capital')

# turn into quantiles
huc_watersheds <- 
  huc_watersheds %>%
  group_by(ecological_landscape) %>%
  # subtract one from quantile rank because it starts at 1 using ntile() function
  # needs to start at 0 to follow the DST
  mutate(cc_rank = ntile(conservation_capital, 4) - 1) %>%
  ungroup()

# view interactively
huc_watersheds %>%
  mapview(zcol = 'cc_rank')

# plot it
huc_cc_rank <- 
  huc_watersheds %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(cc_rank)), color = 'grey') +
  geom_sf(data = eco_landscapes, fill = NA, color = 'black', size = 1) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c('dodgerblue', 'darkolivegreen3', 'orange', 'red'), name = 'CC rank', na.translate = FALSE) +
  ggtitle('CC rank by watershed - expert')


# huc 12s - CO ------------------------------------------------------------

# Calculate vector of mean CO for each watershed
huc_watersheds$conservation_opportunity <- 
  exact_extract(
    co_layer_rescaled, 
    huc_watersheds %>% st_transform(., st_crs(co_layer_rescaled)), 
    'mean'
    )

# no NAs
huc_watersheds %>%
  filter(is.na(conservation_opportunity)) 

# na_co_watersheds <- 
#   huc_watersheds %>%
#   filter(is.na(conservation_opportunity)) %>%
#   pull(huc_id)
# 
# all_na_watersheds <- unique(c(na_cc_watersheds, na_co_watersheds))

# there are 2 NAs here
# huc_watersheds %>%
#   filter(is.na(conservation_opportunity)) %>%
#   mapview()

# drop these slivers?

# plot co ranks by watershed
# ggplot() +
#   geom_sf(data = huc_watersheds, aes(fill = conservation_opportunity)) +
#   scale_fill_viridis_c()

# turn into quantiles
huc_watersheds <- 
  huc_watersheds %>%
  group_by(ecological_landscape) %>%
  # need to subtract 1 as these need to start with 0
  mutate(co_rank = ntile(conservation_opportunity, 4) - 1) %>%
  ungroup()

huc_watersheds %>%
  mapview(zcol = 'co_rank')

huc_co_rank <- 
  huc_watersheds %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(co_rank)), color = 'grey') +
  geom_sf(data = eco_landscapes, fill = NA, color = 'black', size = 1) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c('dodgerblue', 'darkolivegreen3', 'orange', 'red'), name = 'CO rank', na.translate = FALSE) +
  ggtitle('CO rank by watershed - expert')


# priority landscapes -----------------------------------------------------

# create priority landscapes by adding cc + co ranking
# then use rules below to rank landscape on 1-3 scale
eco_landscapes <- 
  eco_landscapes %>%
  mutate(
    priority_rank = cc_rank + co_rank,
    priority_landscape = case_when(
      priority_rank >= 9 ~ 1,
      priority_rank >= 7 & priority_rank <= 8 ~ 2,
      TRUE ~ 3
    )
  )

# this is correct
eco_rank <- 
  eco_landscapes %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(priority_landscape)), color = 'grey') +
  geom_sf(data = eco_landscapes, fill = NA, color = 'black', size = 1) +
  # scale_fill_viridis_d() +
  scale_fill_manual(values = c('red', 'khaki1', 'dodgerblue'), name = 'Priority rank') +
  ggtitle('Priority landscapes - expert')

eco_landscapes %>%
  mapview(zcol = 'priority_rank')

eco_landscapes %>%
  mapview(zcol = 'priority_landscape')

# save
eco_landscapes %>%
  mutate(source = 'expert') %>%
  write_rds(here::here('outputs/eco_landscapes_expert.rds'))

# and to gpkg
eco_landscapes %>%
  mutate(source = 'expert') %>%
  st_write(
    dsn = here::here('outputs/dst_shapefiles.gpkg'),
    layer = 'expert_eco_landscape_rankings',
    layer_options = "OVERWRITE=YES",
    append = FALSE
  )


# final product -----------------------------------------------------------

# add priority landscape rank + CC rank + CO rank
# scores will be on 1-9 scale
final_product <- 
  huc_watersheds %>%
  select(huc_id, ecological_landscape, cc_rank, co_rank) %>% # narrow down columns needed for final layer
  left_join(., eco_landscapes %>% st_drop_geometry() %>% select(ecological_landscape, priority_landscape)) %>% # join ecological landscapes and their priority scores
  # EL scores need to be 'reversed' because all ranks must be in ascending order 
  # highest score = highest priority
  mutate(
    priority_landscape = case_when(
      priority_landscape == 1 ~ 3,
      priority_landscape == 3 ~ 1,
      TRUE ~ 2)
  ) %>%
  mutate(
    source = 'expert',
    priority_watershed = cc_rank + co_rank + priority_landscape
  ) %>% # create final 1-9 score
  select(source, huc_id, ecological_landscape, cc_rank, co_rank, priority_landscape, priority_watershed) # re-organize
hist(final_product$priority_watershed)

final_product %>%
  write_rds(here::here('outputs/priority_watersheds_expert.rds'))

# and to gpkg
final_product %>%
  mutate(source = 'expert') %>%
  st_write(
    dsn = here::here('outputs/dst_shapefiles.gpkg'),
    layer = 'expert_huc_watershed_rankings',
    layer_options = "OVERWRITE=YES",
    append = FALSE
  )


# create a similar color palette to the one used in report/paper
pal <- wes_palette("Zissou1", 9, type = "continuous")

# and a look at final scores
final_rank_expert <- 
  final_product %>%
  ggplot() +
  geom_sf(aes(fill = priority_watershed), color = 'grey', size = 0.1) +
  scale_fill_gradientn(colours = pal, name = 'Priority rank', n.breaks = 9) + 
  ggtitle('expert')

# slivers again
# final_product %>%
#   filter(is.na(priority_watershed)) %>%
#   mapview()

final_product %>%
  mapview(zcol = 'priority_watershed', col.regions = pal)

final_product %>%
  mutate(
    top_watershed = case_when(
      priority_watershed >= 7 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  mapview(zcol = 'top_watershed')

final_product %>%
  mutate(
    top_watershed = case_when(
      priority_watershed >= 7 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ggplot() +
  geom_sf(aes(fill = top_watershed), color = 'grey', size = 0.1) +
  ggtitle('Priority watersheds - expert')

st_layers(dsn = here::here('outputs/dst_shapefiles.gpkg'))

print('script 03 finished')
