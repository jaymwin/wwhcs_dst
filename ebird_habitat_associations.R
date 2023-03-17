
library(sf)
library(ebirdst)
library(tidyverse)
library(rnaturalearth)

# load WI boundary
wi <- 
  ne_states(country = 'united states of america', returnclass = 'sf') %>%
  filter(postal == 'WI')

# or get the path if you already have the data downloaded
path <- get_species_path('buwtea')

# load predictor dependence data
pds <- load_pds(path, model = 'count')

e <- ebirdst_extent(wi, t = c("12-01", "01-31"))

# for testing, run with 5 bootstrap iterations for speed
# in practice, best to run with the default number of iterations (100)
pd_smooth <- plot_pds(pds, "effort_distance_km", ext = e, n_bs = 5)
dplyr::glimpse(pd_smooth)

ebirdst_predictors %>%
  pull(predictor)

?ebirdst::load_pis()

?ebirdst::load_pds()

?ebirdst::plot_pis()

?ebirdst::plot_pds()
