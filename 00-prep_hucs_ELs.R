

# load libraries ----------------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(raster)
library(tmap)
library(exactextractr)
library(patchwork)
library(mapview)
library(wesanderson)

# set select and plotting
select <- dplyr::select
theme_set(theme_minimal())

# other custom functions
source(here::here('99-source_functions.R'))
mapviewOptions(fgb = FALSE) # so we can save map as html


# save WI boundary to gpkg ------------------------------------------------

# projection = NAD83(HARN) / Wisconsin Transverse Mercator
# just going to keep everything in WGS84 (and convert to native raster projection later)
wi <- 
  read_sf(here::here('data/shapefiles/Wisconsin_State_Boundary_24K.shp')) %>%
  st_transform(., crs = 4326)

# save WI
wi %>%
  st_write(
    dsn = here::here('data/dst_spatial.gpkg'),
    layer = 'wi_border',
    layer_options = "OVERWRITE=YES",
    append = FALSE
  )


# ecological landscapes ---------------------------------------------------

# this uses modified EL map from geodatabase as we need a 'Missippi River Zone' which
# was created for this project

# tidy up
eco_landscapes <- 
  read_sf(here::here('data/shapefiles/ecological_landscapes.shp')) %>%
  st_transform(., crs = 4326) %>%
  rename(ecological_landscape = 'ECO_LAND_1') %>%
  # there's a sliver along the Mississippi River that shouldn't be western coulees/ridges
  # fix it here
  st_cast(., "POLYGON") %>%
  mutate(feature_id = as.factor(row_number())) %>%
  mutate(
    ecological_landscape = case_when(
      feature_id == 50 ~ 'Mississippi River Zone',
      TRUE ~ ecological_landscape
    )
  ) %>%
  # now perform a union on EL polygons by eco landscape
  group_by(ecological_landscape) %>%
  summarise()

# these look fine
ggplot() +
  geom_sf(data = eco_landscapes, aes(fill = ecological_landscape))

# nothing unusual here either
ggplot() +
  geom_sf(data = wi) +
  geom_sf(data = eco_landscapes, aes(fill = ecological_landscape)) +
  guides(fill = 'none') +
  facet_wrap(~ecological_landscape)

# last check, interactive map
# eco_landscapes %>%
#   mapview(zcol = 'ecological_landscape')

# save EL layer
eco_landscapes %>%
  st_write(
    dsn = here::here('data/dst_spatial.gpkg'),
    layer = 'eco_landscapes',
    layer_options = "OVERWRITE=YES",
    append = FALSE
  )

# check this worked
st_layers(here::here('data/dst_spatial.gpkg'))


# huc 12 watersheds -------------------------------------------------------

# downloaded from GEE from a spatial extent that goes beyond WI border
huc_watersheds <- 
  read_sf(here::here('data/shapefiles/wisconsin_huc12_watersheds.shp')) %>%
  select(huc_id = 'huc12') %>% 
  group_by(huc_id) %>%
  summarise() %>%
  # get rid of great lakes
  filter(huc_id %ni% c('040203000300', '040602000000'))
# see roughly square extent of watersheds
plot(st_geometry(huc_watersheds))

# read in WI border for cropping
wi_border <- read_sf(here::here('data/dst_spatial.gpkg'), 'wi_border')

# now figure out which watersheds intersect the WI border 
# need to align projections
wi_border_transformed <- 
  wi_border %>% 
  st_transform(., st_crs(huc_watersheds))

# now figure which ones intersect and keep those
huc_watersheds <- 
  huc_watersheds %>%
  # does it intersect?
  mutate(int = st_intersects_any(., wi_border_transformed)) %>%
  # TRUE == yes
  filter(int == TRUE) %>%
  # clean up now
  select(huc_id)

# check that worked; yes
ggplot() +
  geom_sf(data = huc_watersheds) + 
  geom_sf(data = wi_border, fill = NA, color = 'red')

# mapview(wi_border) + mapview(huc_watersheds)

# now do the intersection (cropping), retain huc_id
huc_watersheds <- 
  huc_watersheds %>%
  st_intersection(., wi_border_transformed) %>%
  select(huc_id) %>%
  # now transform project to match ELs and other DST layers
  st_transform(., st_crs(eco_landscapes))

# need to perform polygon union again, final cleaning
huc_watersheds <- 
  huc_watersheds %>%
  group_by(huc_id) %>%
  summarise()

# check; yes
ggplot() +
  geom_sf(data = huc_watersheds) + 
  geom_sf(data = wi_border, fill = NA, color = 'red')

# check again interactively
# huc_watersheds %>%
#   mapview()


# now obtain EL for each watershed

# get watershed centroids
huc_watersheds_centroids <- 
  huc_watersheds %>%
  st_centroid()

# assign ecological landscape based on centroid lat/long
huc_watersheds_centroids <- 
  st_join(
  huc_watersheds_centroids, 
  eco_landscapes, 
  join = st_within
  )

# plot watersheds and their centroids
ggplot() +
  geom_sf(data = huc_watersheds) +
  geom_sf(dat = huc_watersheds_centroids, aes(color = ecological_landscape))

# join eco landscapes from centroids with watershed polygons
huc_watersheds <- 
  huc_watersheds %>%
  left_join(., huc_watersheds_centroids %>% st_drop_geometry())

# plot watersheds by landscape
ggplot() +
  geom_sf(dat = huc_watersheds, aes(fill = ecological_landscape)) +
  facet_wrap(~ecological_landscape)

# where are the NA watersheds occurring?
# huc_watersheds %>% 
#   filter(is.na(ecological_landscape)) %>%
#   mapview()

# filter to them
na_watersheds <- 
  huc_watersheds %>% 
  filter(is.na(ecological_landscape))

# plot and see which EL they should align with
# mapview(eco_landscapes, zcol = 'ecological_landscape') + mapview(na_watersheds)
# 
# na_watersheds %>%
#   filter(huc_id %in% c('071200060907')) %>%
#   mapview()

# mostly northern lake michigan
n_lake_mi_coastal_watersheds <- 
  c(
    '040301020101', 
    '040301080913'
  )

# fix these manually
huc_watersheds <- 
  huc_watersheds %>%
  mutate(
    ecological_landscape = case_when(
      huc_id == '040400020101' ~ 'Southern Lake Michigan Coastal', # near milwaukee/racine
      huc_id %in% n_lake_mi_coastal_watersheds ~ 'Northern Lake Michigan Coastal',
      huc_id == '071200060907' ~ 'Southeast Glacial Plains',
      huc_id == '070400010402' ~ 'Mississippi River Zone',
      TRUE ~ ecological_landscape # otherwise stay the same
    )
  )

# check for NAs again
huc_watersheds %>%
  filter(is.na(ecological_landscape))

# final check
# huc_watersheds %>%
#   mapview(zcol = 'ecological_landscape')

na_watersheds <- c("070400010402", "070900031604", "071200040301", "071200060901")
all_na_watersheds_ebird <- c("071200040301", "071200060901")
distinct_na_watersheds <- unique(c(na_watersheds, all_na_watersheds_ebird))
distinct_na_watersheds

discarded_watersheds <- 
  huc_watersheds %>%
  mutate(
    removed = case_when(
      huc_id %in% distinct_na_watersheds ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(removed == TRUE) %>%
  mapview()
# mapshot(discarded_watersheds, here::here('discarded_watersheds.html'))


original_hucs <- nrow(huc_watersheds)

# lastly, remove these watersheds where no data exists for certain ebird, expert layers
# found this at later steps in the process
# should be 1800 watersheds total
huc_watersheds <-
  huc_watersheds %>%
  # these are missing CO raster cells so end up being NA
  # remove here
  filter(huc_id %ni% distinct_na_watersheds)

final_hucs <- nrow(huc_watersheds)
original_hucs - final_hucs

# plot watersheds by landscape
ggplot() +
  geom_sf(dat = huc_watersheds, aes(fill = ecological_landscape)) +
  facet_wrap(~ecological_landscape)

ggplot() +
  geom_sf(dat = huc_watersheds, aes(fill = ecological_landscape))

# save
huc_watersheds %>%
  st_write(
    dsn = here::here('data/dst_spatial.gpkg'),
    layer = 'huc_watersheds',
    layer_options = "OVERWRITE=YES",
    append = FALSE
  )

# check this worked
st_layers(here::here('data/dst_spatial.gpkg'))


# # create shapefile for hole in winnebago data -----------------------------
# 
# # this occurs in ebird data, have to deal with it in other rasters
# 
# # read in an ebird raster
# r <- raster(here::here('data/ebird_inputs/ebird_cc_inputs/breeding_distribution_fuzzy.tif'))
# plot(r)
# 
# # view the winnebago NA cells
# plot(r, xlim = c(640000, 655000), ylim = c(385000, 400000))
# 
# # create an extent to crop this area
# e <- extent(640000, 655000, 385000, 400000)
# rc <- crop(r, e)	
# plot(rc)
# 
# # now set the lake NAs to 99
# rc[is.na(rc)] <- 99
# plot(rc)
# 
# # convert these 99 cells to polygon
# pol <- rasterToPolygons(rc, fun=function(x){x==99})
# 
# # check
# plot(rc)
# plot(pol, add=TRUE, col='red')
# 
# # perform union
# winnebago_na_polygon <- pol %>%
#   st_as_sf() %>%
#   summarise()
# plot(winnebago_na_polygon)
# 
# # save
# winnebago_na_polygon %>%
#   st_write(
#     dsn = here::here('data/dst_spatial.gpkg'),
#     layer = 'winnebago_hole',
#     layer_options = "OVERWRITE=YES",
#     append = FALSE
#   )
# 
# # check this worked
# st_layers(here::here('data/dst_spatial.gpkg'))

print('script 00 finished')
