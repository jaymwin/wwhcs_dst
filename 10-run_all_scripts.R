
library(tidyverse)
library(fs)

# locate scripts to run analysis
scripts <- 
  dir_ls(here::here(), glob = '*.R') %>%
  as_tibble() %>%
  slice(1:10) %>%
  pull(value)

# run analysis in order
tictoc::tic()
scripts %>%
  walk(., source)
tictoc::toc()
