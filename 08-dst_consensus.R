
library(tidyverse)
library(sf)
library(patchwork)
library(mapview)

select <- dplyr::select
theme_set(theme_minimal())

source(here::here('99-source_functions.R'))


# side by side maps

final_rankings_ebird <-
  read_rds(here::here('outputs/priority_watersheds_ebird.rds')) %>%
  select(huc_id, ecological_landscape, priority_watershed) %>%
  mutate(
    ebird_top = case_when(
      priority_watershed >= 7 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  rename(ebird_rank = priority_watershed)

final_rankings_expert <-
  read_rds(here::here('outputs/priority_watersheds_expert.rds')) %>%
  select(huc_id, ecological_landscape, priority_watershed) %>%
  mutate(
    expert_top = case_when(
      priority_watershed >= 7 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  rename(expert_rank = priority_watershed)

# eco landscapes for plotting
ecol <- 
  eco_landscapes <- read_sf(
  here::here('data/dst_spatial.gpkg'), 
  layer = 'eco_landscapes'
)

# now combine
top_watersheds <- 
  final_rankings_expert %>%
  left_join(., final_rankings_ebird %>% st_drop_geometry()) %>%
  # tidy up
  select(huc_id, ecological_landscape, expert_rank, ebird_rank, expert_top, ebird_top)

# if consensus
top_watersheds <- 
  top_watersheds %>%
  mutate(
    consensus = case_when(
      ebird_top == 1 & expert_top == 1 ~ 1,
      TRUE ~ 0
    )
  )

# percent overlap
perc_overlap_df <- 
  top_watersheds %>%
  # doesn't like to tally as an sf object
  st_drop_geometry() %>%
  # filter watersheds to be 'top' in one way or another
  # either ebird or expert or both
  filter(expert_top == 1 | ebird_top == 1) %>%
  group_by(ecological_landscape, consensus) %>%
  tally() %>%
  # now calculate the toal top watersheds per landscape
  # divide the number overlapping (or not) by the total
  mutate(
    total = sum(n),
    # just proportion for now
    percent_overlap = (n / total)
  ) %>%
  # filter down to the % that are actually shared
  filter(consensus == 1) %>%
  ungroup() %>%
  arrange(percent_overlap)
perc_overlap_df
range(perc_overlap_df$percent_overlap)
hist(perc_overlap_df$percent_overlap)

# now plot this spatially
p_perc_overlap <- 
  perc_overlap_df %>%
  left_join(., ecol) %>%
  st_as_sf() %>%
  st_transform(., 3071) %>%
  ggplot() +
  geom_sf(aes(fill = percent_overlap), size = 0.2) +
  scale_fill_viridis_c(name = 'Agreement in\ntop watersheds', option = 'mako', labels = scales::percent) +
  # scale_fill_viridis_c(name = 'Agreement\nprobability', option = 'A') +
  theme(axis.text = element_blank())


# statewide overlap -------------------------------------------------------

# look across all watersheds in the state now
perc_overlap_statewide <- 
  top_watersheds %>%
  # doesn't like to tally as an sf object
  st_drop_geometry() %>%
  # filter watersheds to be 'top' in one way or another
  # either ebird or expert or both
  filter(expert_top == 1 | ebird_top == 1) %>%
  group_by(consensus) %>%
  tally() %>%
  # now calculate the toal top watersheds per landscape
  # divide the number overlapping (or not) by the total
  mutate(
    total = sum(n),
    # just proportion for now
    percent_overlap = (n / total)
  ) %>%
  # filter down to the % that are actually shared
  filter(consensus == 1)

print('script 08 finished')
