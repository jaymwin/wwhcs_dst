
library(tidyverse)
library(fs)

# locate scripts to run workflow
scripts <- 
  dir_ls(here::here(), glob = '*.R') %>%
  as_tibble() %>%
  slice(1:10) %>%
  pull(value)

# run workflow in order
# takes about ~40 minutes to run everything
tictoc::tic()
scripts %>%
  walk(., source)
tictoc::toc()