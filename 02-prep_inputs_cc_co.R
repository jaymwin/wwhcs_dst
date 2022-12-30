

# load libraries ----------------------------------------------------------

library(rnaturalearth)
library(tidyverse)
library(sf)
library(raster)
library(ebirdst)
library(mapview)
library(fs)
library(furrr)
library(tmap)
# library(tidybayes)
library(patchwork)

# housekeeping stuff
select <- dplyr::select
theme_set(theme_light() + theme(panel.grid.minor = element_blank()))

source(here::here('99-source_functions.R'))

# load wi border
st_layers(here::here('data/dst_spatial.gpkg'))
wi_border <- read_sf(here::here('data/shapefiles/Wisconsin_State_Boundary_24K.shp'))


# deal with outlier ebird counts? -----------------------------------------

dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'rndu') %>%
  as_tibble() %>%
  filter(str_detect(value, 'abundance.tif')) %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack() %>%
  plot() # some of these numbers are high; restrict tails a bit?


# yes, deal with outliers -------------------------------------------------

# find all abundance rasters
abundance_rasters <- dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'abundance.tif') %>%
  as_tibble() %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack()
abundance_rasters

# manipulate to plot count histograms
abundance_rasters_summary <- 
  abundance_rasters %>%
  as.data.frame() %>%
  as_tibble() %>%
  gather(layer, count)

# plot
abundance_rasters_summary %>%
  ggplot() +
  geom_histogram(aes(x =  count)) +
  facet_wrap(~layer, scales = 'free', ncol = 3)

# just to check, no counts less than 0
abundance_rasters_summary %>%
  filter(count < 0)

# ok, some 'extreme' values we might want to remove (or re-classify)
abundance_rasters_summary %>%
  group_by(layer) %>%
  summarise(
    upper = quantile(count, probs = 0.99, na.rm = TRUE)
  )

# look at some different percentiles
intervals <- 
  abundance_rasters_summary %>%
  group_by(layer) %>%
  summarise(
    percentile_90 = quantile(count, probs = c(0.90), na.rm = TRUE),
    percentile_99 = quantile(count, probs = c(0.99), na.rm = TRUE),
    percentile_95 = quantile(count, probs = c(0.95), na.rm = TRUE),
    max_count = max(count, na.rm = TRUE)
  ) %>%
  select(layer, percentile_90, percentile_95, percentile_99, max_count)

intervals <- 
  intervals %>%
  pivot_longer(!layer, names_to = "interval", values_to = "value")

# plot with upper CI
before_outliers <- 
  abundance_rasters_summary %>%
  ggplot() +
  geom_histogram(aes(x =  count)) +
  geom_vline(data = intervals, aes(xintercept = value, color = interval), linetype = 2, size = 0.5) +
  facet_wrap(~layer, scales = 'free', ncol = 3) +
  labs(
    x = 'Predicted count from eBird',
    y = 'Number of raster cells',
    title = 'original data'
    )
before_outliers
ggsave(here::here('figures/truncating_counts_original.png'), width = 8, height = 5)

# calculate upper 99% percentile for each raster layer
intervals <- 
  abundance_rasters_summary %>%
  group_by(layer) %>%
  summarise(
    percentile_99 = quantile(count, probs = 0.99, na.rm = TRUE)
  ) %>%
  select(layer, percentile_99)


# might be wise to restrict counts > upper CI -----------------------------

# create a directory for these truncated (and later scaled) ebird rasters
dir_create(here::here('data/rasters_from_ebirdst_truncated'))

# find abundance rasters
abundance_rasters <- 
  dir_ls(here::here('data/rasters_from_ebirdst'), regexp = 'abundance.tif') %>%
  as_tibble() %>%
  filter(!str_detect(value, '.xml')) %>%
  mutate(layer = str_remove(basename(value), '.tif'))

# grab raster paths
ras <- (abundance_rasters) %>%
  pull(value)

# now loop through each, reclassify high values to upper CI value
for (i in seq_along(ras)) {

  # find raster
  ras_path <- 
    abundance_rasters %>%
    filter(value == ras[i]) %>%
    pull(value)

  # find 'max' count (upper 99% percentile)
  upper <- 
    intervals %>%
    filter(layer == str_remove(basename(ras[i]), '.tif')) %>%
    pull(percentile_99)

  # read in raster
  r <- raster(ras_path)
  plot(r, main = str_c(basename(ras[i]), ' ', 'raw'))
  print(str_c('99th percentile = ', upper))
  print(max(raster::values(r), na.rm = TRUE))

  # recode
  r[r > upper] <- upper

  # check again
  plot(r, main = str_c(basename(ras[i]), ' ', 'recoded'))
  print(max(raster::values(r), na.rm = TRUE))

  # save species x season raster for abundance
  r %>%
    writeRaster(
      .,
      str_c(here::here('data/rasters_from_ebirdst_truncated'), '/', basename(ras[i])),
      overwrite = TRUE
    )

}

# now check this worked
abundance_rasters <- 
  dir_ls(here::here('data/rasters_from_ebirdst_truncated'), regexp = 'abundance.tif') %>%
  as_tibble() %>%
  filter(!str_detect(value, '.xml')) %>%
  pull(value) %>%
  stack()

tm <- 
  tm_shape(abundance_rasters[[4]]) +
  tm_raster(style = 'cont', palette = 'viridis', title = 'Count', legend.reverse = TRUE) +
  tm_layout(main.title = "MALL breeding abund.", main.title.position = "center")
tm
tmap_save(tm, here::here('figures/raw_ebird.png'), width = 3000, height = 3000, dpi = 1000)

# manipulate to plot count histograms
abundance_rasters_summary <- 
  abundance_rasters %>%
  as.data.frame() %>%
  as_tibble() %>%
  gather(layer, count)

intervals_new <- 
  abundance_rasters_summary %>%
  group_by(layer) %>%
  summarise(
    max = max(count, na.rm = TRUE)
  ) %>%
  select(layer, max)

# these should look the same
intervals_new
intervals

# plot; worked
after_outliers <- 
  abundance_rasters_summary %>%
  ggplot() +
  geom_histogram(aes(x =  count)) +
  facet_wrap(~layer, scales = 'free', ncol = 3) +
  labs(
    x = 'Predicted count from eBird',
    y = 'Number of raster cells',
    title = 'outliers removed'
    )
after_outliers
ggsave(here::here('figures/truncating_counts_truncated.png'), width = 8, height = 5)

# view side by side
before_outliers + after_outliers
ggsave(here::here('figures/before_after_truncation.png'), width = 12, height = 5)

# see all rasters again
plot(abundance_rasters)


# create seasonal distributions (breeding, spring, fall) ------------------

# grab a raster from the original DST
hunter_ras <- raster(here::here('data/expert_inputs/expert_cc_inputs/hunter_distribution.tif'))
hunter_ras
# tm_shape(hunter_ras) +
#   tm_raster(style = 'cont', palette = "viridis")

# find abundance rasters
abundance_rasters <- 
  dir_ls(here::here('data/rasters_from_ebirdst_truncated'), regexp = 'abundance.tif') %>%
  as_tibble() %>%
  filter(!str_detect(value, '.xml')) %>%
  mutate(layer = str_remove(basename(value), '.tif'))

# grab raster paths
ras <- (abundance_rasters) %>%
  pull(value)

# now loop through each, reclassify high values to upper 95% CI value
for (i in seq_along(ras)) {
  
  # find raster
  ras_path <- abundance_rasters %>%
    filter(value == ras[i]) %>%
    pull(value)
  
  # read in raster
  r <- raster(ras_path)
  plot(r, main = basename(ras[i]))
  
  # reproject raster to match other DST layers
  r <- projectRaster(r, crs = crs(hunter_ras))
  plot(r, main = basename(ras[i]))
  
  # downscale raster to 1km to match other DST layers
  s <- resample(r, hunter_ras, method = 'bilinear')
  
  # if any counts are now < 0, change that to 0
  s[s < 0] <- 0
  
  # plot again
  plot(s, main = basename(ras[i]))
  
  # put through fuzzy membership
  s_rescaled <- calc(s, fun = rescale_01)
  plot(s_rescaled, main = basename(ras[i]))
  
  # give it a name to save
  raster_name <- ras[i] %>% 
    basename() %>% 
    str_remove('.tif')
  
  # save species x season raster for abundance
  s_rescaled %>%
    writeRaster(
      .,
      str_c(here::here('data/rasters_from_ebirdst_truncated'), '/', raster_name, '_fuzzy.tif'),
      overwrite = TRUE
    )
  
}


# read back in, weighted sum for each season ------------------------------

seasonal_weights_df <- 
  tribble(
  ~species, ~season, ~wt,
  # breeding
  'mall', 'breeding', 0.4,
  'wodu', 'breeding', 0.22, 
  'bwte', 'breeding', 0.11,
  'rndu', 'breeding', 0.05,
  # fall
  'mall', 'fall', 0.32,
  'wodu', 'fall', 0.22, 
  'bwte', 'fall', 0.07,
  'rndu', 'fall', 0.03,
  # spring
  'mall', 'spring', 0.48,
  'wodu', 'spring', 0.20, 
  'bwte', 'spring', 0.17,
  'rndu', 'spring', 0.14,
)

fuzzy_abundance <- 
  dir_ls(here::here('data/rasters_from_ebirdst_truncated'), glob = '*fuzzy.tif') %>%
  stack()
plot(fuzzy_abundance)

names_to_keep <- c(
  'bwte_breeding',
  'bwte_fall',
  'bwte_spring',
  'mall_breeding',
  'mall_fall',
  'mall_spring',
  'rndu_breeding',
  'rndu_fall',
  'rndu_spring',
  'wodu_breeding',
  'wodu_fall',
  'wodu_spring'
) %>%
  str_flatten(collapse = '|')

names(fuzzy_abundance) <- 
  names(fuzzy_abundance) %>% 
  str_extract(names_to_keep)

tm <- tm_shape(fuzzy_abundance) +
  tm_raster(style = 'cont', palette = 'viridis', title = 'Value', legend.reverse = TRUE) +
  tm_shape(wi_border %>% st_transform(st_crs(fuzzy_abundance))) +
  tm_borders() +
  tm_facets(ncol = 3)
tm
tmap_save(tm, here::here('figures/ebird_distributions.png'), width = 5000, height = 6000, dpi = 1000)

fuzzy_abundance <- 
  dir_ls(here::here('data/rasters_from_ebirdst_truncated'), glob = '*fuzzy.tif') %>%
  as_tibble() %>%
  mutate(
    season = str_extract(value, 'breeding|fall|spring'),
    species = str_extract(value, 'bwte|mall|rndu|wodu')
  )


seasons <- c('breeding', 'fall', 'spring')

for (i in seq_along(seasons)) {
  
  # breeding
  # (e.g., 40% for mallard, 22% for wood duck, 11% for blue-winged teal, 5% for ring-necked duck).
  season_layers <- 
    fuzzy_abundance %>%
    filter(season == seasons[i]) %>%
    left_join(., seasonal_weights_df)

  season_stack <- 
    season_layers %>%
    pull(value) %>%
    stack()

  print(seasons[i])
  print(names(season_stack))
  print(season_layers$wt)
  
  season_distribution <- 
    weighted_sum(
    stack = season_stack,
    w = season_layers$wt
  )
  plot(season_distribution)
  
  season_distribution_rescaled <- calc(season_distribution, fun = rescale_01)
  plot(season_distribution_rescaled)
  
  season_distribution_rescaled %>%
    writeRaster(
      .,
      str_c(here::here('data/ebird_inputs/ebird_cc_inputs'), '/', seasons[i], '_distribution_fuzzy.tif'),
      overwrite = TRUE
    )
  
}


# create breeding habitat potential layer ---------------------------------

# use breeding distribution on 0-1 scale
breeding_season_distribution <- raster(str_c(here::here('data/ebird_inputs/ebird_cc_inputs'), '/', 'breeding_distribution_fuzzy.tif'))
plot(breeding_season_distribution)

# divide into quantiles
values_to_remove <- 
  raster::values(breeding_season_distribution) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(quant = ntile(value, 32)) %>%
  filter(quant >= 30) %>% # remove top 3 quantiles (30, 31, 32)
  arrange(value) %>%
  slice(1) %>%
  pull(value)

# reclassify top 3 quantiles into 0 (remove core habitat)
breeding_season_distribution[breeding_season_distribution >= values_to_remove] <- 0
plot(breeding_season_distribution)

prw <- raster(here::here('data/expert_inputs/expert_co_inputs/potentially_restorable_wetlands.tif'))
prw
plot(prw)

plot(stack(breeding_season_distribution, prw))

# multiply core removed by prw
breeding_potential <- raster::overlay(
  breeding_season_distribution, 
  prw, 
  fun = raster_product
)
breeding_potential
plot(breeding_potential)

plot(stack(breeding_season_distribution, prw, breeding_potential))


# hist(raster::values(breeding_potential))
# 
# breeding_potential_rescaled <- calc(breeding_potential, fun = rescale_01)
# plot(breeding_potential_rescaled)
# 
# tm_shape(breeding_potential_rescaled) +
#   tm_raster(style = 'cont', palette = 'viridis')
# 
# hist(raster::values(breeding_potential_rescaled))
# 
# # ok, some 'extreme' values we might want to remove (or re-classify)
# breeding_potential %>%
#   raster::values(.) %>%
#   median_qi(na.rm = TRUE)
# 
# # calculate X% CIs
# upper_interval <- breeding_potential %>%
#   raster::values(.) %>%
#   tidybayes::point_interval(.width = 0.95, na.rm = TRUE) %>%
#   pull(ymax)
# 
# breeding_potential[breeding_potential > upper_interval] <- upper_interval
# plot(breeding_potential)

breeding_potential_rescaled <- calc(breeding_potential, fun = rescale_01)
plot(breeding_potential_rescaled)

# tm_shape(breeding_potential_rescaled) +
#   tm_raster(style = 'cont', palette = 'viridis')
# 
# s <- stack(c(breeding_potential_rescaled, raster(str_c(here::here('data/ebird_inputs/ebird_cc_inputs'), '/', 'breeding_distribution_fuzzy.tif'))))
# tm_shape(s) +
#   tm_raster(style = 'cont', palette = 'viridis')

# save breeding potential raster
breeding_potential_rescaled %>%
  writeRaster(
    .,
    str_c(here::here('data/ebird_inputs/ebird_co_inputs'), '/', 'breeding_habitat_potential_fuzzy.tif'),
    overwrite = TRUE
  )

print('script 02 finished')

