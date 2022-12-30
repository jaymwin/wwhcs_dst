
library(tidyverse)
library(ggokabeito)   # Neat accessible color palette
library(tidybayes)
library(brms)
library(fs)
library(sf)
library(raster)
library(mapview)
library(wesanderson)
library(patchwork)
library(exactextractr)
library(tidytext)
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


# prepare rasters ---------------------------------------------------------

# ebird seasonal distributions
ebird_rasters <- 
  dir_ls(here::here('data/ebird_inputs/ebird_cc_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'distribution_fuzzy'))

# pull paths
ebird_rasters_paths <- 
  ebird_rasters %>%
  pull(value)

# stack and plot
ebird_distribution_stack <- 
  ebird_rasters_paths %>% 
  stack()
plot(ebird_distribution_stack)

# expert seasonal distributions
expert_rasters <- 
  dir_ls(here::here('data/expert_inputs/expert_cc_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'expert'))

# pull paths
expert_rasters_paths <- 
  expert_rasters %>%
  pull(value)

# stack and plot
expert_distribution_stack <- 
  expert_rasters_paths %>% 
  stack()
plot(expert_distribution_stack)

# set names of stack
names(expert_distribution_stack) <- 
  c(
  'fall_distribution_expert', 
  'breeding_distribution_expert', 
  'spring_distribution_expert'
  )


# huc_watersheds <- huc_watersheds %>%
#   slice(1:3) %>%
#   group_by(huc_id) %>%
#   nest() %>%
#   ungroup() %>%
#   # do this for each seasonal distribution
#   expand_grid(
#     ., 
#     season =  c(
#       'breeding', 
#       'fall',
#       'spring'
#     )
#   ) %>%
#   mutate(
#     # crop and calculate median value for each watershed by expert and ebird
#     expert = map2(data, season, crop_raster_expert),
#     ebird = map2(data, season, crop_raster_ebird)
#   ) %>%
#   unnest()
# huc_watersheds


huc_watersheds <- 
  huc_watersheds %>%
  # do this for each seasonal distribution
  expand_grid(
    ., 
    season =  c(
      'breeding', 
      'fall',
      'spring'
    )
  ) %>%
  st_as_sf() %>%
  group_split(season)

# breeding - expert
huc_watersheds[[1]]$expert <- 
  exact_extract(
    expert_distribution_stack[[2]], 
    huc_watersheds[[1]], 
    'median'
  )

# fall - expert
huc_watersheds[[2]]$expert <- 
  exact_extract(
    expert_distribution_stack[[1]], 
    huc_watersheds[[2]], 
    'median'
  )

# spring - expert
huc_watersheds[[3]]$expert <- 
  exact_extract(
    expert_distribution_stack[[3]], 
    huc_watersheds[[3]], 
    'median'
  )

# breeding - ebird
huc_watersheds[[1]]$ebird <- 
  exact_extract(
    ebird_distribution_stack[[1]], 
    huc_watersheds[[1]], 
    'median'
  )

# fall - ebird
huc_watersheds[[2]]$ebird <- 
  exact_extract(
    ebird_distribution_stack[[2]], 
    huc_watersheds[[2]], 
    'median'
  )

# spring - ebird
huc_watersheds[[3]]$ebird <- 
  exact_extract(
    ebird_distribution_stack[[3]], 
    huc_watersheds[[3]], 
    'median'
  )

# re-combine
huc_watersheds <- 
  huc_watersheds %>%
  bind_rows()

huc_watersheds %>%
  filter(season == 'breeding') %>%
  ggplot() +
  geom_point(aes(expert, ebird)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(~ecological_landscape)

# save
huc_watersheds %>%
  st_drop_geometry() %>%
  write_rds(here::here('outputs/huc_watersheds_median_distribution_value.rds'))

# # read back in
# huc_watersheds <-
#   read_rds(here::here('outputs/huc_watersheds_median_distribution_value.rds'))


# calculate correlations (watershed value by ecoregion) ------------

# prepare to calculate rho (correlation) in brms
huc_watersheds <- 
  huc_watersheds %>%
  # do so by landscape and season
  group_by(ecological_landscape, season) %>%
  nest()

# fit models, extract posterior
huc_watersheds <- 
  huc_watersheds %>%
  mutate(
    # standardize values first
    data = map(data, standardize_values),
    # fit model for each ecological landscape
    fit = map(data, fit_brm),
    # extract posterior samples from each model fit
    post = map(fit, get_post)
  )

# save again
huc_watersheds %>%
  ungroup() %>%
  select(-data:-fit) %>%
  write_rds(here::here('outputs/cc_correlations.rds'))

# huc_watersheds <- read_rds(here::here('outputs/cc_correlations.rds'))

# see what these correlation values look like; mean
huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  group_by(ecological_landscape, season) %>%
  mean_qi(value) %>%
  pull(value) %>%
  mean()

# all
huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  group_by(ecological_landscape, season) %>%
  mean_qi(value) %>%
  arrange(season, value) %>%
  print(n=Inf)

# all considered 'strongly' correlated
huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  group_by(ecological_landscape, season) %>%
  median_qi(value) %>%
  arrange(season, value) %>%
  filter(value >= 0.7)


# # plot correlations -------------------------------------------------------
# 
# # posterior distributions first
# # example plot
# example_plot <- 
#   huc_watersheds %>%
#   ungroup() %>%
#   unnest(post) %>%
#   filter(season == 'breeding') %>%
#   mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
#   ggplot(aes(x = value, y = ecological_landscape)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, fill = NA, point_size = 1, point_color = NA, interval_color = NA) +
#   labs(x = expression(rho), y = 'Ecological landscape') +
#   geom_vline(aes(xintercept = 0), linetype = 2) +
#   guides(fill = 'none') +
#   xlim(c(-1, 1)) +
#   theme_minimal(base_size = 14) +
#   theme(panel.grid.minor = element_blank(), axis.text.y = element_blank())
# example_plot
# ggsave(here::here('figures/rho_example.png'), height = 6, width = 4, units = 'in', dpi = 1000)
# 
# # breeding correlations
# b_plot <- 
#   huc_watersheds %>%
#   ungroup() %>%
#   unnest(post) %>%
#   filter(season == 'breeding') %>%
#   mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
#   ggplot(aes(x = value, y = ecological_landscape)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, fill = palette_okabe_ito(order = 3), point_size = 1) +
#   labs(x = NULL, y = 'Ecological landscape') +
#   geom_vline(aes(xintercept = 0), linetype = 2) +
#   guides(fill = 'none') +
#   xlim(c(-1, 1)) +
#   ggtitle('Breeding') +
#   theme_minimal(base_size = 14) +
#   theme(panel.grid.minor = element_blank(), axis.text = element_blank())
# b_plot
# ggsave(here::here('figures/rho_breeding.png'), height = 6, width = 4, units = 'in', dpi = 1000)
# 
# # fall correlations
# f_plot <- 
#   huc_watersheds %>%
#   ungroup() %>%
#   unnest(post) %>%
#   filter(season == 'fall') %>%
#   mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
#   ggplot(aes(x = value, y = ecological_landscape, fill = season)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, fill = palette_okabe_ito(order = 5), point_size = 1) +
#   scale_fill_okabe_ito() +
#   labs(x = expression(rho), y = NULL) +
#   geom_vline(aes(xintercept = 0), linetype = 2) +
#   guides(fill = 'none') +
#   xlim(c(-1, 1)) +
#   ggtitle('Fall') +
#   theme_minimal(base_size = 14) +
#   theme(panel.grid.minor = element_blank(), axis.text = element_blank())
# f_plot
# ggsave(here::here('figures/rho_fall.png'), height = 6, width = 4, units = 'in', dpi = 1000)
# 
# # spring correlations
# s_plot <- 
#   huc_watersheds %>%
#   ungroup() %>%
#   unnest(post) %>%
#   filter(season == 'spring') %>%
#   mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
#   ggplot(aes(x = value, y = ecological_landscape, fill = season)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, fill = palette_okabe_ito(order = 6), point_size = 1) +
#   scale_fill_okabe_ito() +
#   labs(x = NULL, y = NULL) +
#   geom_vline(aes(xintercept = 0), linetype = 2) +
#   guides(fill = 'none') +
#   xlim(c(-1, 1)) +
#   ggtitle('Spring') +
#   theme_minimal(base_size = 14) +
#   theme(panel.grid.minor = element_blank(), axis.text = element_blank())
# s_plot
# ggsave(here::here('figures/rho_spring.png'), height = 6, width = 4, units = 'in', dpi = 1000)
# 
# # all seasons combined plot
# posterior_correlations <- 
#   huc_watersheds %>%
#   ungroup() %>%
#   unnest(post) %>%
#   # group_by(season) %>%
#   # filter(season == 'fall') %>%
#   # mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
#   mutate(
#     ecological_landscape = reorder_within(season, value, ecological_landscape)
#   ) %>%
#   ggplot(aes(x = value, y = ecological_landscape)) +
#   stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, point_size = 1) +
#   # scale_fill_okabe_ito() +
#   scale_y_reordered() +
#   labs(x = expression(rho), y = 'Ecological landscape') +
#   geom_vline(aes(xintercept = 0), linetype = 2) +
#   guides(fill = 'none') +
#   xlim(c(-1, 1)) +
#   # ggtitle('Fall') +
#   # theme_minimal(base_size = 14) +
#   theme(panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
#   facet_wrap(~str_to_title(season), scales = 'free_y')
# # ggsave(here::here('figures/rho_season.png'), height = 6, width = 12, units = 'in', dpi = 1000)
# 
# # now plot spatially the median correlation value
# eco_correlations <- 
#   read_sf(
#     here::here('data/dst_spatial.gpkg'), 
#     layer = 'eco_landscapes'
#   )
# 
# # join to huc_watersheds df
# eco_correlations <- 
#   eco_correlations %>%
#   left_join(
#     ., 
#     huc_watersheds %>%
#       unnest(post) %>%
#       group_by(ecological_landscape, season) %>%
#       summarise(correlation = mean(value))
#   )
# 
# eco_correlations %>%
#   print(n=Inf)
# 
# # plot and save
# 
# # use range for scale limits
# range(eco_correlations$correlation)
# 
# spatial_correlation_plot <- 
#   eco_correlations %>%
#   st_transform(., 3071) %>%
#   mutate(season = str_to_sentence(season)) %>%
#   ggplot() +
#   geom_sf(aes(fill = correlation), size = 0.2) +
#   geom_sf(data = wi_border %>% st_transform(st_crs(eco_correlations)), fill = NA, size = 0.3) +
#   scale_fill_scico(palette = 'bam', name = expression(rho), limits = c(-0.60, 0.80), breaks = c(-0.5, 0, 0.5)) +
#   # scale_fill_scico(palette = 'roma', name = expression(rho), direction = -1) +
#   # scale_fill_gradientn(colours = c("blue", "white", "red"), name = expression(rho), limits = c(-0.8, 0.8)) +
#   facet_wrap(~season) +
#   theme(
#     axis.text = element_blank(),
#     # strip.background = element_blank(),
#     # strip.text.x = element_blank(),
#     # legend.position = 'bottom'
#   )
# # ggsave(here::here('figures/ecoregion_corr_map.png'), height = 2.5, width = 6, units = 'in', dpi = 600)
# 
# posterior_correlations / spatial_correlation_plot + plot_annotation(tag_levels = 'A')
# ggsave(here::here('figures/post_cor_and_map.png'), height = 5.5, width = 6, units = 'in', dpi = 600)

print('script 07 finished')
