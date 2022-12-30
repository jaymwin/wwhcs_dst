
library(tidyverse)
library(fs)
library(sf)
library(raster)
library(mapview)
library(stars)
library(scico)

select <- dplyr::select
theme_set(theme_minimal())

source(here::here('99-source_functions.R'))

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

el <- read_sf(
  here::here('data/dst_spatial.gpkg'), 
  layer = 'eco_landscapes'
)


# locate ebird abundance rasters (fuzzy) ----------------------------------

ebird_abundance_rasters <- dir_ls(here::here('data/rasters_from_ebirdst_truncated'), glob = '*abundance_fuzzy.tif')

# pull out species and season; for example here with bwte in breeding season
spp <- basename(ebird_abundance_rasters[1]) %>% 
  str_extract(., 'bwte|mall|rndu|wodu')

seas <- basename(ebird_abundance_rasters[1]) %>% 
  str_extract(., 'breeding|fall|spring')

# create new raster name (species_season)
ras_name <- str_c(spp, '_', seas)

# read in and stack expert habitat suitability rasters
hab_density_stack <- 
  dir_ls(here::here('data/rasters_from_gdb'), glob = '*.tif') %>%
  stack()
plot(hab_density_stack)

# rename expert rasters
names(hab_density_stack)
names(hab_density_stack) <- c(
  'bwte_fall',
  'bwte_breeding',
  'bwte_spring',
  'mall_fall',
  'mall_breeding',
  'mall_spring',
  'rndu_fall',
  'rndu_breeding',
  'rndu_spring',
  'wodu_fall',
  'wodu_breeding',
  'wodu_spring'
)

# use that name to subset habitat density stack
# this example should subset bwte breeding from expert
# works
plot(subset(hab_density_stack, ras_name))


# calculate species x season differences ----------------------------------

# mylist <- list() # create an empty list

# create empty list to store difference rasters
raslist <- list()

# loop through each ebird raster path
for (i in seq_along(ebird_abundance_rasters)) {
  
  # for each ebird abundance raster, pull out species, season name
  
  # species
  spp <- basename(ebird_abundance_rasters[i]) %>% 
    str_extract(., 'bwte|mall|rndu|wodu')
  
  # season
  seas <- basename(ebird_abundance_rasters[i]) %>% 
    str_extract(., 'breeding|fall|spring')
  
  # combine; this will subset expert raster of same name
  ras_name <- str_c(spp, '_', seas)
  print(ras_name)
  
  # use it to subset habitat density stack
  wi_dst_ras <- subset(hab_density_stack, ras_name)
  
  # then load the ebird raster
  ebird_ras <- raster(ebird_abundance_rasters[i])
  
  # now calculate difference between the two (ebird - expert)
  r <- raster::overlay(
    ebird_ras, 
    wi_dst_ras, 
    fun = calc_diff
  )
  plot(r, main = str_glue({spp}, '_', {seas}))
  names(r) <- str_glue({spp}, '_', {seas})
  
  # # save difference values as data frame
  # p2 <- as.data.frame(r, xy = TRUE) %>% # s is your raster file %>%
  #   as_tibble() %>%
  #   drop_na() %>%
  #   mutate(species = spp, season = seas) %>%
  #   rename(difference = 3)
  
  # mylist[[i]] <- p2 # put all vectors in the list
  raslist[[i]] <- r # put all vectors in the list
  
}

# df <- do.call("rbind", mylist) # combine all vectors into a matrix
# df

# stack all of the difference rasters
diff_rasters <- do.call("stack", raslist)

# works
plot(diff_rasters[[2]])

# diff_rasters[[2]] %>%
#   mapview()

# tm <- tm_shape(diff_rasters) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = FALSE) +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 6)
# tmap_save(tm, here::here('figures/diff_map.png'), width = 8000, height = 4000, dpi = 1000)

# save these difference rasters
diff_rasters %>%
  write_rds(here::here('outputs/species_difference_rasters.rds'))

# tm <- tm_shape(diff_rasters) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tmap_save(tm, here::here('figures/diff_map.png'), width = 3000, height = 3000, dpi = 600)
# 
# diff_rasters
# names(diff_rasters) <- str_to_title(names(diff_rasters))

# plot these differences
# easiest as a stars object
# diff_rasters_stars <- diff_rasters %>%
#   st_as_stars()
# 
# # now plot
# ggplot() +
#   geom_stars(data = diff_rasters_stars) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_rasters_stars)), fill = NA, size = 0.2) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_rasters_stars)), fill = NA, size = 0.3) +
#   facet_wrap(~band, ncol = 3) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   # scale_fill_distiller(
#   scale_fill_scico(
#     # palette = 'RdYlGn', 
#     palette = 'bam',
#     name = NULL, 
#     na.value = NA, 
#     direction = 1, 
#     # needed for proper scale gradient
#     limits = c(min(raster::values(diff_rasters), na.rm = TRUE), max(raster::values(diff_rasters), na.rm = TRUE)),
#     breaks = c(min(raster::values(diff_rasters), na.rm = TRUE), 0, max(raster::values(diff_rasters), na.rm = TRUE)), 
#     labels = c('>Expert', 'No difference', '>eBird')
#   )
# ggsave(here::here('figures/diff_map.png'), width = 5, height = 6, dpi = 600, units = 'in')

# tm <- tm_shape(diff_rasters[[1:3]]) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tm
# tmap_save(tm, here::here('figures/diff_map_bwte.png'), width = 6000, height = 3000, dpi = 600)

# individual species; bwte
# diff_bwte <- diff_rasters[[1:3]] %>%
#   st_as_stars()
# 
# ggplot() +
#   geom_stars(data = diff_bwte) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   facet_wrap(~band) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   # scale_fill_distiller(palette = 'RdYlGn', name = 'Difference', na.value = NA, direction = 1) +
#   scale_fill_scico(
#     # palette = 'RdYlGn', 
#     palette = 'bam',
#     name = NULL, 
#     na.value = NA, 
#     direction = 1, 
#     # needed for proper scale gradient
#     limits = c(min(raster::values(diff_rasters[[1:3]]), na.rm = TRUE), max(raster::values(diff_rasters[[1:3]]), na.rm = TRUE)),
#     breaks = c(min(raster::values(diff_rasters[[1:3]]), na.rm = TRUE), 0, max(raster::values(diff_rasters[[1:3]]), na.rm = TRUE)),
#     labels = c('>Expert', 'No difference', '>eBird')
#   )
# ggsave(here::here('figures/diff_map_bwte.png'), height = 3, width = 6, units = 'in', dpi = 600)

# tm <- tm_shape(diff_rasters[[4:6]]) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tm
# tmap_save(tm, here::here('figures/diff_map_mall.png'), width = 6000, height = 3000, dpi = 600)

# diff_mall <- diff_rasters[[4:6]] %>%
#   st_as_stars()
# 
# ggplot() +
#   geom_stars(data = diff_mall) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   facet_wrap(~band) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   scale_fill_distiller(palette = 'RdYlGn', name = 'Difference', na.value = NA, direction = 1)
# ggsave(here::here('figures/diff_map_mall.png'), height = 3, width = 6, units = 'in', dpi = 600)

# tm <- tm_shape(diff_rasters[[7:9]]) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tm
# tmap_save(tm, here::here('figures/diff_map_rndu.png'), width = 6000, height = 3000, dpi = 600)

# diff_rndu <- diff_rasters[[7:9]] %>%
#   st_as_stars()
# 
# ggplot() +
#   geom_stars(data = diff_rndu) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   facet_wrap(~band) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   scale_fill_distiller(palette = 'RdYlGn', name = 'Difference', na.value = NA, direction = 1)
# ggsave(here::here('figures/diff_map_rndu.png'), height = 3, width = 6, units = 'in', dpi = 600)

# tm <- tm_shape(diff_rasters[[10:12]]) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tm
# tmap_save(tm, here::here('figures/diff_map_wodu.png'), width = 6000, height = 3000, dpi = 600)

# diff_wodu <- diff_rasters[[10:12]] %>%
#   st_as_stars()
# 
# ggplot() +
#   geom_stars(data = diff_wodu) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA) +
#   facet_wrap(~band) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   scale_fill_distiller(palette = 'RdYlGn', name = 'Difference', na.value = NA, direction = 1)
# ggsave(here::here('figures/diff_map_wodu.png'), height = 3, width = 6, units = 'in', dpi = 600)


# do something similar for CC inputs --------------------------------------

# these are weighted sums of breeding/fall/spring
ebird_distribution_rasters <- 
  dir_ls(
    here::here('data/ebird_inputs/ebird_cc_inputs'), 
    glob = '*distribution_fuzzy.tif'
    )

# just season now
seas <- basename(ebird_distribution_rasters[1]) %>%
  str_extract(., 'breeding|fall|spring')
ras_name <- str_c(seas)

# seasonal suitability layers
hab_distribution_stack <- 
  dir_ls(
  here::here('data/expert_inputs/expert_cc_inputs'), 
  glob = '*distribution_expert.tif'
  ) %>%
  stack()

# hab_distribution_stack[[1]] %>%
#   mapview()
# 
# hab_distribution_stack[[3]] %>%
#   mapview()
# 
# hab_distribution_stack[[2]] %>%
#   mapview()

# make sure expert, ebird have the same names
names(hab_distribution_stack)
names(hab_distribution_stack) <- 
  c(
  'fall',
  'breeding',
  'spring'
)
plot(hab_distribution_stack)

# use that name to subset habitat density stack, breeding here
plot(subset(hab_distribution_stack, ras_name))

# mylist <- list() # create an empty list

# create empty list to store difference rasters
raslist <- list()

# create empty list to store difference rasters
for (i in seq_along(ebird_distribution_rasters)) {
  
  # for each ebird abundance raster, pull out species, season name
  seas <- basename(ebird_distribution_rasters[i]) %>% 
    str_extract(., 'breeding|fall|spring')
  ras_name <- str_c(seas)
  print(ras_name)
  
  # use it to subset habitat density stack
  wi_dst_ras <- subset(hab_distribution_stack, ras_name)
  
  # then load the ebird raster
  ebird_ras <- raster(ebird_distribution_rasters[i])
  
  # now calculate difference between the two (ebird - expert)
  r <- 
    raster::overlay(
    ebird_ras, 
    wi_dst_ras, 
    fun = calc_diff
  )
  plot(r, main = str_glue({seas}))
  names(r) <- str_glue({seas})
  
  # save as data frame
  # p2 <- as.data.frame(r, xy = TRUE) %>% # s is your raster file %>%
  #   as_tibble() %>%
  #   drop_na() %>%
  #   mutate(season = seas) %>%
  #   rename(difference = 3)
  
  # mylist[[i]] <- p2 # put all vectors in the list
  raslist[[i]] <- r # put all vectors in the list
  
}

# df <- do.call("rbind", mylist) # combine all vectors into a matrix
# df
# 
# df %>%
#   ggplot() +
#   geom_histogram(aes(x = difference, fill = season))

# df %>%
#   mutate(season = reorder(season, difference)) %>%
#   ggplot(aes(x = difference, y = season, fill = season)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = median_qi) +
#   scale_fill_okabe_ito() +
#   guides(fill = 'none')

# stack all of the difference rasters
diff_rasters_seasonal <- do.call("stack", raslist)

diff_rasters_seasonal %>%
  write_rds(here::here('outputs/seasonal_difference_rasters.rds'))

# tm <- tm_shape(diff_rasters_seasonal) +
#   tm_raster(title = "Difference", style = "cont", palette = "RdYlGn", legend.show = TRUE) +
#   tm_shape(el %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_shape(wi_border %>% st_transform(st_crs(diff_rasters))) +
#   tm_borders() +
#   tm_facets(ncol = 3)
# tm
# tmap_save(tm, here::here('figures/diff_map_seasons.png'), width = 6000, height = 3000, dpi = 600)

# diff_rasters_seasonal[[1]] %>%
#   mapview()
# 
# 
# diff_rasters_seasonal[[2]] %>%
#   mapview()
# 
# 
# diff_rasters_seasonal[[3]] %>%
#   mapview()

# diff_rasters_seasonal
# names(diff_rasters_seasonal) <- str_to_title(names(diff_rasters_seasonal))
# 
# # plot as a stars object
# diff_rasters_seasonal_stars <- diff_rasters_seasonal %>%
#   st_as_stars()
# 
# ggplot() +
#   geom_stars(data = diff_rasters_seasonal_stars) +
#   geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA, size = 0.2) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA, size = 0.3) +
#   facet_wrap(~band) +
#   theme_minimal() +
#   theme(axis.text = element_blank(), axis.title = element_blank()) +
#   # scale_fill_distiller(
#   #   palette = 'RdYlGn', 
#   scale_fill_scico(
#     palette = 'bam',
#     name = NULL, 
#     na.value = NA, 
#     direction = 1, 
#     limits = c(min(raster::values(diff_rasters), na.rm = TRUE), max(raster::values(diff_rasters), na.rm = TRUE)),
#     breaks = c(min(raster::values(diff_rasters), na.rm = TRUE), 0, max(raster::values(diff_rasters), na.rm = TRUE)), 
#     labels = c('>Expert', 'No difference', '>eBird')
#     )
# ggsave(here::here('figures/diff_map_seasons.png'), height = 2.5, width = 6, units = 'in', dpi = 600)

print('script 06 finished')
