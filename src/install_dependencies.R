pck <- c(
  'data.table',
  'doParallel',
  'dplyr',
  'foreach',
  'ggplot2',
  'ggstance',
  'glm2',
  'glue',
  'gridExtra',
  'here',
  'ISOweek',
  'lubridate',
  'MASS',
  'mgcv',
  'prismatic',
  'purrr',
  'scales',
  'showtext',
  'smooth',
  'tidyverse',
  'yaml'
)

install.packages(
  pck,
  dep = TRUE
)

install.packages(
  'INLA',
  repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/stable'),
  dep = TRUE
)
