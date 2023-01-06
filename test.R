
# find CC inputs which have already been through fuzzy membership
breeding_hab_density_inputs <- 
  dir_ls(here::here('data/rasters_from_gdb'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'spring'))
breeding_hab_density_inputs

# pull paths
breeding_hab_density_stack_paths <- 
  breeding_hab_density_inputs %>%
  pull(value)
breeding_hab_density_stack_paths

# stack raster layers and plot
breeding_hab_density_stack <- 
  breeding_hab_density_stack_paths %>% 
  stack() %>%
  crop(., wi_border %>% st_transform(., st_crs(breeding_hab_density_stack))) %>%
  mask(., wi_border %>% st_transform(., st_crs(breeding_hab_density_stack)))
breeding_hab_density_stack
plot(breeding_hab_density_stack)

# # (e.g., 40%
# #   for mallard, 22% for wood duck, 11% for blue-winged
# #   teal, 5% for ring-necked duck)
# 
# # define weights (these follow the order of rasters in the stack)
# breeding_weights <- 
#   breeding_hab_density_inputs %>%
#   mutate(
#     wt = c(
#       0.11, # bwte
#       0.40, # mall
#       0.05, # rndu
#       0.22 # wodu
#     )
#   )
# 
# # n (e.g., 32% for mallard, 22% for
# #    wood duck, 7% for blue-winged teal, 3% for ringnecked duck).
# 
# # define weights (these follow the order of rasters in the stack)
# breeding_weights <- 
#   breeding_hab_density_inputs %>%
#   mutate(
#     wt = c(
#       0.07, # bwte
#       0.32, # mall
#       0.03, # rndu
#       0.22 # wodu
#     )
#   )

# [e.g., 48% for mallard,
#   20%, for wood duck, 17% for blue-winged teal, 14%
#   for ring-necked duck
  

# autumn ------------------------------------------------------------------

# find CC inputs which have already been through fuzzy membership
autumn_hab_density_inputs <- 
  dir_ls(here::here('data/rasters_from_gdb'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'autumn'))
autumn_hab_density_inputs

# pull paths
autumn_hab_density_stack_paths <- 
  autumn_hab_density_inputs %>%
  pull(value)
autumn_hab_density_stack_paths

# stack raster layers and plot
autumn_hab_density_stack <- 
  autumn_hab_density_stack_paths %>% 
  stack()

autumn_hab_density_stack <- 
  autumn_hab_density_stack %>%
  raster::crop(., wi_border %>% st_transform(., st_crs(autumn_hab_density_stack))) %>%
  raster::mask(., wi_border %>% st_transform(., st_crs(autumn_hab_density_stack)))
autumn_hab_density_stack
plot(autumn_hab_density_stack)

autumn_weights <- 
  autumn_hab_density_inputs %>%
  mutate(
    wt = c(
      0.07, # bwte
      0.32, # mall
      0.03, # rndu
      0.22 # wodu
    )
  )

# doesn't add to 1
sum(autumn_weights$wt)

# perform weighted sum and plot
autumn_layer <- 
  weighted_sum(
    stack = autumn_hab_density_stack,
    w = autumn_weights$wt
  )
autumn_layer

autumn_layer_rescaled <- calc(autumn_layer, fun = rescale_01)

# plot the one I have
cc_inputs <- 
  dir_ls(here::here('data/expert_inputs/expert_cc_inputs'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'hunter|ecological|expert'))
cc_inputs

# pull paths
cc_inputs_paths <- 
  cc_inputs %>%
  pull(value)

# stack raster layers and plot
cc_inputs_stack <- 
  cc_inputs_paths %>% 
  stack()
cc_inputs_stack

plot(stack(autumn_layer_rescaled, cc_inputs_stack[[1]]))

r1 <- raster::as.data.frame(autumn_layer_rescaled) %>%
  tibble() %>%
  rename(mine = layer)

r2 <- raster::as.data.frame(cc_inputs_stack[[1]]) %>%
  tibble() %>%
  rename(expert = autumn_distribution_expert)

ggplot() +
  geom_point(data = r1 %>% bind_cols(., r2), aes(expert, mine), alpha = 2/10) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2, color = 'orange', size = 1) +
  ggtitle('Autumn')


# spring ------------------------------------------------------------------

# find CC inputs which have already been through fuzzy membership
spring_hab_density_inputs <- 
  dir_ls(here::here('data/rasters_from_gdb'), glob = '*.tif') %>%
  as_tibble() %>%
  mutate(raster = basename(value)) %>%
  filter(str_detect(raster, 'spring'))
spring_hab_density_inputs

# pull paths
spring_hab_density_stack_paths <- 
  spring_hab_density_inputs %>%
  pull(value)
spring_hab_density_stack_paths

# stack raster layers and plot
spring_hab_density_stack <- 
  spring_hab_density_stack_paths %>% 
  stack()

spring_hab_density_stack <- 
  spring_hab_density_stack %>%
  crop(., wi_border %>% st_transform(., st_crs(spring_hab_density_stack))) %>%
  mask(., wi_border %>% st_transform(., st_crs(spring_hab_density_stack)))
spring_hab_density_stack
plot(spring_hab_density_stack)

# define weights (these follow the order of rasters in the stack)
spring_weights <- 
  spring_hab_density_inputs %>%
  mutate(
    wt = c(
      0.17, # bwte
      0.48, # mall
      0.14, # rndu
      0.20 # wodu
    )
  )
sum(spring_weights$wt)

# perform weighted sum and plot
spring_layer <- 
  weighted_sum(
    stack = spring_hab_density_stack,
    w = spring_weights$wt
  )
spring_layer

spring_layer_rescaled <- calc(spring_layer, fun = rescale_01)

# plot the one I have
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

plot(stack(spring_layer_rescaled, cc_inputs_stack[[5]]))

r1 <- raster::as.data.frame(spring_layer_rescaled) %>%
  tibble() %>%
  rename(mine = layer)

r2 <- raster::as.data.frame(cc_inputs_stack[[5]]) %>%
  tibble() %>%
  rename(expert = spring_distribution_expert)

ggplot() +
  geom_point(data = r1 %>% bind_cols(., r2), aes(expert, mine), alpha = 2/10) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2, color = 'tomato', size = 1) +
  ggtitle('Spring')
