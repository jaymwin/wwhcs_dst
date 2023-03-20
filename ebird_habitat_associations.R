
library(sf)
library(ebirdst)
library(tidyverse)
library(rnaturalearth)

# load WI boundary
wi <- 
  ne_states(country = 'united states of america', returnclass = 'sf') %>%
  filter(postal == 'WI')

# or get the path if you already have the data downloaded
path <- get_species_path('mallar3')

predictors <- 
  ebirdst_predictors %>%
  filter(str_detect(predictor, 'mcd12q1|astwbd')) %>% 
  filter(!lc_class_label %in% c('Ocean', 'Barren', 'Tundra', 'Unclassified', 'Permanent Snow and Ice', 'Evergreen Needleleaf Forests')) %>%
  pull(predictor)

# load predictor dependence data
pds <- 
  load_pds(path, model = 'count') %>%
  filter(predictor %in% c(predictors))

e <- ebirdst_extent(wi, t = c("05-01", "08-15"))

hab_predictors <-
  ebirdst_predictors %>%
  filter(str_detect(predictor, 'mcd12q1|astwbd')) %>% 
  filter(!lc_class_label %in% c('Ocean', 'Barren', 'Tundra', 'Unclassified', 'Permanent Snow and Ice', 'Evergreen Needleleaf Forests'))


for (i in seq_along(predictors)) {
  
  print(predictors[i])
  
  df <- 
    hab_predictors %>%
    filter(predictor == predictors[i])
  
  pred_label <- df %>% pull(predictor_label)
  pred_label <- pred_label %>% str_replace(., '/', '-')
  
  # for testing, run with 5 bootstrap iterations for speed
  # in practice, best to run with the default number of iterations (100)
  try(
    pd_smooth <- 
      plot_pds(
        pds, 
        predictors[i], 
        ext = e, 
        # n_bs = 5, 
        bootstrap_smooth = FALSE,
        plot = FALSE
      )
  )
  
  try(
    pd_smooth %>%
      ggplot() +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_ribbon(aes(x, ymin = pd_lower, ymax = pd_upper), alpha = 6/10, fill = 'grey') +
      geom_line(aes(x, pd_median), size = 1.5) +
      labs(
        x = NULL,
        y = 'Deviation E(Count)',
        title = str_c('Mallard - ', pred_label)
      ) +
      theme_light()
  )
  
  try(
    ggsave(str_c(here::here('ebird_partial_dependence_plots/mall - '), pred_label, '.png'), width = 5, height = 4, units = 'in')
  )
  
}

