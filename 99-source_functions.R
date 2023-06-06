
select <- dplyr::select

'%ni%' <- Negate('%in%')

# weighted sum function to add rasters
# multiply raster by weight, then add
weighted_sum <- function(stack, w) {
  
  # multiply raster values by raster-specific weights
  r <- stack * w
  
  # sum raster stack
  r <- raster::calc(r, fun = sum, na.rm = FALSE) # won't add layers if NA exists in any of them
  return(r)
  
}


# # fuzzy membership alternative 
# rescale_01 <- function(x) {
#   
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#   
# }


# fuzzy membership alternative 
rescale_01 <- function(x) {
  
  x / max(x, na.rm = TRUE)
  
}


# fit brms correlation models
fit_brm <- function(x) {
  
  brm(
    data = x, 
    family = student,
    bf(mvbind(expert, ebird) ~ 1) + set_rescor(TRUE),
    # prior = c(prior(gamma(2, .1), class = nu),
    #           prior(normal(0, 100), class = Intercept, resp = x),
    #           prior(normal(0, 100), class = Intercept, resp = y),
    #           prior(normal(0, 100), class = sigma, resp = x),
    #           prior(normal(0, 100), class = sigma, resp = y),
    #           prior(lkj(1), class = rescor)),
    # iter = 2000, 
    # warmup = 500, 
    chains = 4, 
    cores = 4, 
    backend = 'cmdstanr',
    seed = 210191
  )
  
}


# extract posterior from brms model fit
get_post <- function(x) {
  
  x %>%
    posterior_samples() %>%
    tidyr::gather(variable, value) %>%
    filter(variable == 'rescor__expert__ebird')
  
}


# extract posterior from brms model fit (agreement model)
get_post_agreement <- function(x) {
  
  x %>%
    posterior_samples() %>%
    tidyr::gather(variable, value) %>%
    filter(variable == 'b_Intercept')
  
}


# standardize variables prior to fitting brms models
standardize_values <- function(x) {
  
  x %>%
    mutate(
      expert = (expert - mean(expert, na.rm = TRUE)) / sd(expert, na.rm = TRUE),
      ebird = (ebird - mean(ebird, na.rm = TRUE)) / sd(ebird, na.rm = TRUE)
    )
  
}

# crop ebird raster to a specific watershed
# then calculate median value in watershed
crop_raster_ebird <- function(x, y) {
  
  focal <- ebird_raster_names %>%
    filter(str_detect(value, y)) %>%
    pull(value)
  
  ras <- subset(ebird_distribution_stack, focal)
  
  raster::crop(ras, x) %>% # crop to section extent
    raster::mask(., x) %>% # mask to section border
    raster::values(.) %>%
    median(., na.rm = TRUE)
  
}


# crop expert raster to a specific watershed
crop_raster_expert <- function(x, y) {
  
  focal <- expert_raster_names %>%
    filter(str_detect(value, y)) %>%
    pull(value)
  
  ras <- subset(expert_distribution_stack, focal)
  
  raster::crop(ras, x) %>% # crop to section extent
    raster::mask(., x) %>% # mask to section border
    raster::values(.) %>%
    median(., na.rm = TRUE)
  
}


# basically fuzzy product function
raster_product <- function(x, y) {
  return(x * y) # this will return NA if one layer has NA
}


# use st_intersects custom function to do so
st_intersects_any <- function(x, y) {
  st_intersects(x, y) %>%
    map_lgl(~length(.x) > 0)
}


# ebird - expert
calc_diff <- function(x, y) {
  return(x - y)
}


# crop study-wide landcover raster to a particular section
crop_expert_raster <- function(x) {
  
  raster::crop(expert_breeding, x) %>% # crop to section extent
    raster::mask(., x) # mask to section border
  
}


crop_ebird_raster <- function(x) {
  
  raster::crop(ebird_breeding, x) %>% # crop to section extent
    raster::mask(., x) # mask to section border
  
}


# write a function to download ebird data in parallel
bulk_download <- function(species) {
  
  ebirdst_download(
    species =  species,
    tifs_only = FALSE,
    force = TRUE,
    show_progress = FALSE
  )
  
}


# probability of agreement model
fit_accuracy_model <- function(df) {
  
  brm(
    data = df, 
    family = bernoulli(identity),
    shared ~ 1,
    prior(
      beta(1, 1), 
      class = Intercept, 
      lb = 0, 
      ub = 1
    ),
    cores = 4,
    backend = 'cmdstanr', 
    seed = 450
  )
  
}