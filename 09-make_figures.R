

# load libraries ----------------------------------------------------------

library(ggsflabel)
library(tidyverse)
library(sf)
library(raster)
library(scico)
library(stars)
library(patchwork)
library(tidybayes)
library(tidytext)

source(here::here('99-source_functions.R'))


# study area map ----------------------------------------------------------

# 453
set.seed(999)

huc_watersheds <- 
  read_sf(
    here::here('data/dst_spatial.gpkg'), 
    layer = 'huc_watersheds'
  )

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
eco_landscapes

r <- sample(17, replace = FALSE)
r

eco_landscapes$r <- as.factor(r)

eco_centroid <- eco_landscapes %>%
  st_transform(., 3071)
eco_centroid

xy = st_coordinates(st_centroid(eco_centroid))
eco_centroid = eco_centroid[order(xy[,"Y"], xy[,"X"]),]

eco_centroid <- eco_centroid %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  mutate(name = as.factor(row_number()))
eco_centroid

blah <- tibble(
  ecological_landscape = 'Superior Coastal Plain',
  r = as.factor(3),
  name = as.factor(17)
) %>%
  mutate(
    geometry <- st_geometry(eco_centroid[eco_centroid$ecological_landscape == "Superior Coastal Plain", ]) + c(0, 50000)
  ) %>%
  st_as_sf()
blah
st_crs(blah) <- 3071
blah <- blah %>%
  rename(geometry = `geometry <- ...`)

eco_centroid <- eco_centroid %>%
  filter(ecological_landscape != 'Superior Coastal Plain') %>%
  bind_rows(., blah)
eco_centroid

# A -> B
points_labels <- data.frame(
  foo = c('A', 'B'), 
  long = c(422469.7, 393746.9), 
  lat = c(733578.1, 372820.9)
) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 3071) 
points_labels

points_polygons <- data.frame(
  bar = c('B', 'D'), 
  long = c(422469.7 + 30000, 393746.9 + 22000), 
  lat = c(708578, 372820.9 + 20000)
) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 3071) 

cbind(points_labels, points_polygons) -> df
df

coords <- cbind(st_coordinates(df$geometry), st_coordinates(df$geometry.1))

linestrings <- 
  st_sfc(
    lapply(1:nrow(coords),
           function(i){
             st_linestring(matrix(coords[i,],ncol=2,byrow=TRUE))
           })
  )
linestrings
st_crs(linestrings) <- 3071

p_eco <- 
  eco_landscapes %>%
  st_transform(., 3071) %>%
  ggplot() +
  geom_sf(aes(fill = r), size = 0.2) +
  scale_fill_viridis_d(name = 'Ecological Landscape') +
  geom_sf(data = linestrings, size = 0.25) +
  geom_sf(data = eco_centroid, pch = 21, color = 'black', fill = 'white', size = 4, stroke = 0.2) +
  geom_sf_text(data = eco_centroid, aes(label = name), size = 2) +
  guides(fill = 'none') +
  # ggtitle('A)') +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())
p_eco

p_huc <- 
  huc_watersheds %>%
  left_join(., eco_landscapes %>% st_drop_geometry() %>% dplyr::select(ecological_landscape, r)) %>%
  st_transform(., 3071) %>%
  ggplot() +
  geom_sf(aes(fill = r), size = 0.2) +
  scale_fill_viridis_d(name = 'Ecological Landscape') +
  guides(fill = 'none') +
  # ggtitle('B)') +
  theme_minimal() +
  theme(axis.text = element_blank())

p_eco + p_huc + plot_annotation(tag_levels = 'A')
ggsave(here::here('figures/study_area.png'), height = 3, width = 5, units = 'in', dpi = 600)


# species x season difference maps ----------------------------------------

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

# save these difference rasters
diff_rasters <-
  read_rds(here::here('outputs/species_difference_rasters.rds'))

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
diff_rasters_stars <- diff_rasters %>%
  st_as_stars()

# dose.labs <- c("spring", "breeding", "fall", "blah", "blah", "blah", "blah", "blah", "blah", "blah", "blah", "blah")
# length(dose.labs)

diff_rasters_stars
str(diff_rasters_stars)

# now plot
ggplot() +
  geom_stars(data = diff_rasters_stars) +
  geom_sf(data = el %>% st_transform(st_crs(diff_rasters_stars)), fill = NA, size = 0.2) +
  geom_sf(data = wi_border %>% st_transform(st_crs(diff_rasters_stars)), fill = NA, size = 0.3) +
  facet_wrap(~band, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank(),
    strip.text = element_blank()
    ) +
  # scale_fill_distiller(
  scale_fill_scico(
    # palette = 'RdYlGn', 
    palette = 'bam',
    name = NULL, 
    na.value = NA, 
    direction = 1, 
    # needed for proper scale gradient
    limits = c(min(raster::values(diff_rasters), na.rm = TRUE), max(raster::values(diff_rasters), na.rm = TRUE)),
    breaks = c(min(raster::values(diff_rasters), na.rm = TRUE), 0, max(raster::values(diff_rasters), na.rm = TRUE)), 
    labels = c('>Expert', 'No difference', '>eBird')
  )
ggsave(here::here('figures/diff_map.png'), width = 5, height = 6, dpi = 600, units = 'in')

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
diff_bwte <- diff_rasters[[1:3]] %>%
  st_as_stars()

ggplot() +
  geom_stars(data = diff_bwte) +
  geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA) +
  geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA) +
  facet_wrap(~band) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank()) +
  # scale_fill_distiller(palette = 'RdYlGn', name = 'Difference', na.value = NA, direction = 1) +
  scale_fill_scico(
    # palette = 'RdYlGn', 
    palette = 'bam',
    name = NULL, 
    na.value = NA, 
    direction = 1, 
    # needed for proper scale gradient
    limits = c(min(raster::values(diff_rasters[[1:3]]), na.rm = TRUE), max(raster::values(diff_rasters[[1:3]]), na.rm = TRUE)),
    breaks = c(min(raster::values(diff_rasters[[1:3]]), na.rm = TRUE), 0, max(raster::values(diff_rasters[[1:3]]), na.rm = TRUE)),
    labels = c('>Expert', 'No difference', '>eBird')
  )
ggsave(here::here('figures/diff_map_bwte.png'), height = 3, width = 6, units = 'in', dpi = 600)


# season difference maps --------------------------------------------------

diff_rasters_seasonal <-
  read_rds(here::here('outputs/seasonal_difference_rasters.rds'))

names(diff_rasters_seasonal) <- str_to_title(names(diff_rasters_seasonal))

# plot as a stars object
diff_rasters_seasonal_stars <- diff_rasters_seasonal %>%
  st_as_stars()

ggplot() +
  geom_stars(data = diff_rasters_seasonal_stars) +
  geom_sf(data = el %>% st_transform(st_crs(diff_bwte)), fill = NA, size = 0.2) +
  geom_sf(data = wi_border %>% st_transform(st_crs(diff_bwte)), fill = NA, size = 0.3) +
  facet_wrap(~band) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank()) +
  # scale_fill_distiller(
  #   palette = 'RdYlGn', 
  scale_fill_scico(
    palette = 'bam',
    name = NULL, 
    na.value = NA, 
    direction = 1, 
    limits = c(min(raster::values(diff_rasters), na.rm = TRUE), max(raster::values(diff_rasters), na.rm = TRUE)),
    breaks = c(min(raster::values(diff_rasters), na.rm = TRUE), 0, max(raster::values(diff_rasters), na.rm = TRUE)), 
    labels = c('>Expert', 'No difference', '>eBird')
  )
ggsave(here::here('figures/diff_map_seasons.png'), height = 2.5, width = 6, units = 'in', dpi = 600)


# plot correlations -------------------------------------------------------

# read in correlations
huc_watersheds <- read_rds(here::here('outputs/cc_correlations.rds'))

# posterior distributions first
# example plot
example_plot <- 
  huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  filter(season == 'breeding') %>%
  mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
  ggplot(aes(x = value, y = ecological_landscape)) +
  stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, fill = NA, point_size = 1, point_color = NA, interval_color = NA) +
  labs(x = expression(rho), y = 'Ecological landscape') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  guides(fill = 'none') +
  xlim(c(-1, 1)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank())
example_plot
ggsave(here::here('figures/rho_example.png'), height = 6, width = 4, units = 'in', dpi = 1000)

# breeding correlations
b_plot <- 
  huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  filter(season == 'breeding') %>%
  mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
  ggplot(aes(x = value, y = ecological_landscape)) +
  stat_halfeye(.width = c(0.95), point_interval = mean_qi, point_size = 1) +
  labs(x = NULL, y = 'Ecological landscape') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  guides(fill = 'none') +
  xlim(c(-1, 1)) +
  ggtitle('Breeding') +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), axis.text = element_blank())
b_plot
ggsave(here::here('figures/rho_breeding.png'), height = 6, width = 4, units = 'in', dpi = 1000)

# fall correlations
f_plot <- 
  huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  filter(season == 'fall') %>%
  mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
  ggplot(aes(x = value, y = ecological_landscape, fill = season)) +
  stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, point_size = 1) +
  labs(x = expression(rho), y = NULL) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  guides(fill = 'none') +
  xlim(c(-1, 1)) +
  ggtitle('Fall') +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), axis.text = element_blank())
f_plot
ggsave(here::here('figures/rho_fall.png'), height = 6, width = 4, units = 'in', dpi = 1000)

# spring correlations
s_plot <- 
  huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  filter(season == 'spring') %>%
  mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
  ggplot(aes(x = value, y = ecological_landscape, fill = season)) +
  stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, point_size = 1) +
  labs(x = NULL, y = NULL) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  guides(fill = 'none') +
  xlim(c(-1, 1)) +
  ggtitle('Spring') +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(), axis.text = element_blank())
s_plot
ggsave(here::here('figures/rho_spring.png'), height = 6, width = 4, units = 'in', dpi = 1000)

# all seasons combined plot
posterior_correlations <- 
  huc_watersheds %>%
  ungroup() %>%
  unnest(post) %>%
  # group_by(season) %>%
  # filter(season == 'fall') %>%
  # mutate(ecological_landscape = reorder(ecological_landscape, value)) %>%
  mutate(
    ecological_landscape = reorder_within(season, value, ecological_landscape)
  ) %>%
  ggplot(aes(x = value, y = ecological_landscape)) +
  stat_halfeye(.width = c(0, 0.95), point_interval = mean_qi, point_size = 1) +
  # scale_fill_okabe_ito() +
  scale_y_reordered() +
  # expression(bar(rho)))
  labs(x = expression(rho), y = 'Ecological landscape') +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  guides(fill = 'none') +
  xlim(c(-1, 1)) +
  # ggtitle('Fall') +
  # theme_minimal(base_size = 14) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
  facet_wrap(~str_to_title(season), scales = 'free_y')
# ggsave(here::here('figures/rho_season.png'), height = 6, width = 12, units = 'in', dpi = 1000)

# now plot spatially the median correlation value
eco_correlations <- 
  read_sf(
    here::here('data/dst_spatial.gpkg'), 
    layer = 'eco_landscapes'
  )

# join to huc_watersheds df
eco_correlations <- 
  eco_correlations %>%
  left_join(
    ., 
    huc_watersheds %>%
      unnest(post) %>%
      group_by(ecological_landscape, season) %>%
      summarise(correlation = mean(value))
  )

eco_correlations %>%
  print(n=Inf)

# use range for scale limits
range(eco_correlations$correlation)
limit <- max(abs(eco_correlations$correlation)) * c(-1, 1)

spatial_correlation_plot <- 
  eco_correlations %>%
  st_transform(., 3071) %>%
  mutate(season = str_to_sentence(season)) %>%
  ggplot() +
  geom_sf(aes(fill = correlation), size = 0.2) +
  geom_sf(data = wi_border %>% st_transform(st_crs(eco_correlations)), fill = NA, size = 0.3) +
  # scale_fill_scico(palette = 'bam', name = expression(rho), limits = c(-0.60, 0.80), breaks = c(-0.5, 0, 0.5)) +
  scale_fill_scico(palette = 'cork', name = expression(bar(rho)), limit = limit) +
  # scale_fill_scico(palette = 'roma', name = expression(rho), direction = -1) +
  # scale_fill_gradientn(colours = c("blue", "white", "red"), name = expression(rho), limits = c(-0.8, 0.8)) +
  facet_wrap(~season) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    # strip.background = element_blank(),
    # strip.text.x = element_blank(),
    # legend.position = 'bottom'
  )
# ggsave(here::here('figures/ecoregion_corr_map.png'), height = 2.5, width = 6, units = 'in', dpi = 600)

posterior_correlations / spatial_correlation_plot + plot_annotation(tag_levels = 'A')
ggsave(here::here('figures/post_cor_and_map.png'), height = 5.5, width = 6, units = 'in', dpi = 600)

spatial_correlation_plot
ggsave(here::here('figures/spatial_correlation_plot_presentation.png'), height = 2.5, width = 6, units = 'in')

posterior_correlations
ggsave(here::here('figures/posterior_correlations_presentation.png'), height = 3, width = 6, units = 'in')


# dst ranking maps --------------------------------------------------------

# ebird
final_product_ebird <-
  read_rds(here::here('outputs/priority_watersheds_ebird.rds'))

# expert
final_product_expert <-
  read_rds(here::here('outputs/priority_watersheds_expert.rds'))

# create dataset with source (expert or ebird, huc ID, EL, ranks)
final_product <- 
  final_product_ebird %>%
  bind_rows(., final_product_expert) %>%
  mutate(
    source = case_when(
      source == 'expert' ~ str_to_title(source),
      TRUE ~ source
    ),
    source = fct_relevel(source, rev)
  )

# add top rankings too in different object
top_final_product <- 
  final_product %>%
  mutate(
    top_watershed = case_when(
      priority_watershed >= 7 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# eco landscapes for plotting
ecol <- 
  read_sf(
    here::here('data/dst_spatial.gpkg'), 
    layer = 'eco_landscapes'
  )

# side by side maps
p1 <- 
  final_product %>%
  st_transform(., 3071) %>%
  ggplot() +
  geom_sf(aes(fill = priority_watershed), color = 'grey', size = 0.1) +
  # scale_fill_gradientn(colours = pal, name = 'Priority rank', n.breaks = 9) + 
  # scale_fill_gradientn(
  #   name = 'Priority rank',
  #   colours = pal, 
  #   breaks = seq(1, 9, length.out = 9),
  #   labels = seq(1, 9, length.out = 9)
  # ) +
  scale_fill_viridis_c(
    name = 'Priority\nrank', 
    breaks = seq(1, 9, length.out = 9), 
    option = 'plasma'
  ) +
  guides(fill = guide_legend(
    # title.position = "right", direction = "vertical",
    # title.theme = element_text(angle = 90, size = 12, colour = "black"),
    # barheight = .25, barwidth = .95,
    keywidth = 1/2,
    keyheight = 1/2,
    # title.hjust = 0.5, raster = FALSE,
    # title = 'Priority rank',
    reverse = TRUE)
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_wrap(~source)
p1
ggsave(here::here('figures/ranks_expert_ebird.png'), width = 5, height = 3, units = 'in')

# side by side (top ranked only)
ggplot() +
  geom_sf(data = top_final_product %>% filter(top_watershed == FALSE) %>% st_transform(., 3071), fill = NA, color = 'grey', size = 0.1) +
  geom_sf(data = top_final_product %>% filter(top_watershed == TRUE) %>% st_transform(., 3071), aes(fill = top_watershed), color = 'grey', size = 0.1) +
  geom_sf(data = ecol %>% st_transform(., 3071), fill = NA, color = 'black', size = 0.4) +
  # scale_fill_manual(values = c('white', '#009E73'), name = 'Top\nwatershed') +
  ggokabeito::scale_fill_okabe_ito(order = c(3), name = 'Top\nwatershed') +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_wrap(~source) # +
# guides(fill = 'none')
ggsave(here::here('figures/top_ranks_expert_ebird.png'), width = 5, height = 3, units = 'in')

p2 <- 
  top_final_product %>%
  st_drop_geometry() %>%
  group_by(priority_watershed, source) %>%
  tally() %>%
  ggplot(
    aes(
      x = as.factor(priority_watershed), y = n, 
      fill = source,
      label = n
    )
  ) +
  geom_col(position = "dodge") +
  geom_text(
    position = position_dodge(width = 0.9),
    hjust = -0.25,
    size = 2
  ) +
  scale_fill_viridis_d(option = 'E', name = 'DST') +
  theme_minimal() +
  # + other title / theme options
  ylim(c(0, 330)) +
  coord_flip() +
  labs(
    y = 'Number of watersheds',
    x = 'Priority rank'
  )
p2
ggsave(here::here('figures/compare_rankings.png'), units = 'in', height = 5, width = 8)

# combined rankings map with bar plot of rankings by dst
p1 / p2 + plot_annotation(tag_levels = 'A')
ggsave(here::here('figures/comparing_DSTs.png'), units = 'in', height = 5, width = 6)



# expert habitat suitability maps -----------------------------------------

# species x season expert rasters
expert_cc_inputs <- 
  dir_ls(here::here('data/rasters_from_gdb'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster_name = basename(value)) %>%
  slice(1)

# pull paths
expert_cc_inputs_paths <- 
  expert_cc_inputs %>%
  pull(value)

# stack raster layers and plot
expert_cc_inputs_stack <- 
  expert_cc_inputs_paths %>% 
  raster()
plot(expert_cc_inputs_stack)

bwte_autumn_map <-expert_cc_inputs_stack %>%
  mapview::mapview()

mapshot(bwte_autumn_map, str_c(here::here(), '/', 'bwte_autumn_map.html'))



#' library(tidyverse)
#' 
#' 
#' install.packages('ggnewscale')
#' 
#' library(easypackages)
#' easypackages::packages("sf",
#'                        "raster",
#'                        "stars",
#'                        "r5r",
#'                        "geobr",
#'                        "aopdata",
#'                        "gtfs2gps",
#'                        "ggplot2",
#'                        "osmdata",
#'                        "h3jsr",
#'                        "viridisLite",
#'                        "ggnewscale",
#'                        "dplyr",
#'                        "magrittr",
#'                        prompt = FALSE
#' )
#' 
#' rotate_data <- function(data, x_add = 0, y_add = 0) {
#'   
#'   shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
#'   
#'   rotate_matrix <- function(x){ 
#'     matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
#'   }
#'   data %>% 
#'     dplyr::mutate(
#'       geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
#'     )
#' }
#' 
#' rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
#'   shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
#'   
#'   rotate_matrix <- function(x) { 
#'     matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
#'   }
#'   data %>% 
#'     dplyr::mutate(
#'       geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
#'     )
#' }
#' 
#' ### get terrain data ----------------
#' 
#' # read terrain raster and calculate hill Shade
#' dem <- stars::read_stars(system.file("extdata/poa/poa_elevation.tif", package = "r5r"))
#' dem <- st_as_sf(dem)
#' 
#' # crop
#' bbox <- st_bbox(dem)
#' 
#' 
#' ### get public transport network data ----------------
#' 
#' gtfs <- gtfs2gps::read_gtfs( system.file("extdata/poa/poa.zip", package = "r5r") )
#' gtfs <- gtfs2gps::gtfs_shapes_as_sf(gtfs)
#' 
#' # crop
#' gtfs <- gtfs[bbox,]
#' gtfs <- st_crop(gtfs, bbox)
#' plot(gtfs['shape_id'])
#' 
#' 
#' ### get OSM data ----------------
#' 
#' # roads from OSM
#' roads <- opq('porto alegre') %>%
#'   add_osm_feature(key = 'highway',
#'                   value = c("motorway", "primary","secondary")) %>% osmdata_sf()
#' 
#' roads <- roads$osm_lines
#' 
#' # crop
#' roads2 <- roads[bbox,]
#' roads2 <- st_crop(roads2, bbox)
#' plot(roads2['osm_id'])
#' 
#' 
#' ### get H3 hexagonal grid ----------------
#' 
#' # get poa muni and hex ids
#' poa <- read_municipality(code_muni = 4314902 )
#' hex_ids <- h3jsr::polyfill(poa, res = 7, simple = TRUE)
#' 
#' # pass h3 ids to return the hexagonal grid
#' hex_grid <- h3jsr::h3_to_polygon(hex_ids, simple = FALSE)
#' plot(hex_grid)
#' 
#' # crop
#' hex_grid <- hex_grid[bbox,]
#' hex <- st_crop(hex_grid, bbox)
#' plot(hex)
#' 
#' 
#' ### get land use data from AOP project ----------------
#' #' more info at https://www.ipea.gov.br/acessooportunidades/en/
#' 
#' landuse <- aopdata::read_access(city = 'poa', geometry = T, mode='public_transport')
#' 
#' # crop
#' landuse <- landuse[bbox,]
#' landuse <- st_crop(landuse, bbox)
#' plot(landuse['CMATT30'])
#' 
#' # hospitals
#' # generate one point per hospital in corresponding hex cells
#' df_temp <- subset(landuse, S004>0)
#' hospitals <- st_sample(x = df_temp, df_temp$S004, by_polygon = T)
#' hospitals <- st_sf(hospitals)
#' hospitals$geometry <- st_geometry(hospitals)
#' hospitals$hospitals <- NULL
#' hospitals <- st_sf(hospitals)
#' plot(hospitals)
#' 
#' # schools
#' # generate one point per schools in corresponding hex cells
#' df_temp <- subset(landuse, E001>0)
#' schools <- st_sample(x = df_temp, df_temp$E001, by_polygon = T)
#' schools <- st_sf(schools)
#' schools$geometry <- st_geometry(schools)
#' schools$schools <- NULL
#' schools <- st_sf(schools)
#' plot(schools)
#' 
#' ### plot  ----------------
#' 
#' # annotate parameters
#' x = -141.25
#' color = 'gray40'
#' 
#' temp1 <- ggplot() +
#'   
#'   # terrain
#'   geom_sf(data = dem %>% rotate_data(), aes(fill=poa_elevation.tif), color=NA, show.legend = FALSE) +
#'   scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#'   annotate("text", label='Terrain', x=x, y= -8.0, hjust = 0, color=color) +
#'   labs(caption = "image by @UrbanDemog")
#' 
#' temp1
#' 
#' temp2 <- temp1 +
#'   
#'   # pop income
#'   new_scale_fill() + 
#'   new_scale_color() +
#'   geom_sf(data = subset(landuse,P001>0) %>% rotate_data(y_add = .1), aes(fill=R001), color=NA, show.legend = FALSE) +
#'   scale_fill_viridis_c(option = 'E') +
#'   annotate("text", label='Population', x=x, y= -7.9, hjust = 0, color=color) +
#'   
#'   # schools
#'   geom_sf(data = hex %>% rotate_data(y_add = .2), color='gray50', fill=NA, size=.1) +
#'   geom_sf(data = schools %>% rotate_data(y_add = .2), color='#0f3c53', size=.1, alpha=.8) +
#'   annotate("text", label='Schools', x=x, y= -7.8, hjust = 0, color=color) +
#'   
#'   # hospitals
#'   geom_sf(data = hex %>% rotate_data(y_add = .3), color='gray50', fill=NA, size=.1) +
#'   geom_sf(data = hospitals %>% rotate_data(y_add = .3), color='#d5303e', size=.1, alpha=.5) +
#'   annotate("text", label='Hospitals', x=x, y= -7.7, hjust = 0, color=color) +
#'   
#'   # OSM
#'   geom_sf(data = roads2 %>% rotate_data(y_add = .4), color='#019a98', size=.2) +
#'   annotate("text", label='Roads', x=x, y= -7.6, hjust = 0, color=color) +
#'   
#'   # public transport
#'   geom_sf(data = gtfs %>% rotate_data(y_add = .5), color='#0f3c53', size=.2) +
#'   annotate("text", label='Public transport', x=x, y= -7.5, hjust = 0, color=color) +
#'   
#'   # accessibility
#'   new_scale_fill() + 
#'   new_scale_color() +
#'   geom_sf(data = subset(landuse, P001>0) %>% rotate_data(y_add = .6), aes(fill=CMATT30), color=NA, show.legend = FALSE) +
#'   scale_fill_viridis_c(direction = 1, option = 'viridis' ) +
#'   theme(legend.position = "none") +
#'   annotate("text", label='Accessibility', x=x, y= -7.4, hjust = 0, color=color) +
#'   theme_void() +
#'   scale_x_continuous(limits = c(-141.65, -141.1))
#' 
#' temp2
#' # save plot
#' ggsave(plot = temp2, filename = 'map_layers.png', 
#'        dpi=200, width = 15, height = 16, units='cm')
#' 
#' 
#' 
#' bwte <- diff_rasters[[2]] %>%
#'   st_as_stars() %>%
#'   st_as_sf()
#' bwte
#' 
#' wis <- wi_border %>% st_transform(., st_crs(bwte)) %>% rename(geometry = geom)
#' wis
#' 
#' ggplot() +
#'   
#'   # terrain
#'   geom_sf(data = bwte %>% rotate_data(y_add = 0), aes(fill=bwte_fall), color=NA, show.legend = FALSE) +
#'   annotate("text", label = 'Breeding distribution', x=2200000, y = 2e5, hjust = 0) +
#'   scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#'   
#'   geom_sf(data = wis %>% rotate_data(y_add = 2e5)) +
#'   theme_void() +
#'   coord_sf(xlim = c(1000000, 3000000))
#' 
#' ggsave(here::here('map_layers.png'), 
#'        dpi=1000, width = 5, height = 4, units='in')

print('script 09 finished')
