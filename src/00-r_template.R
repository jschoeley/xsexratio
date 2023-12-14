# What does the script do?
#
# How does the script do it?

# Init ------------------------------------------------------------

library(yaml)
library(tidyverse)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './src/config.yaml',
  excess = 'tmp/excess_deaths.rds',
  global_constants = 'src/00-global_objects.R'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  data = './dat/output_data.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- within(list(), {

})

# global functions and constants
source(paths$input$global_constants)

# list containers for analysis artifacts
dat <- list()

# Import ----------------------------------------------------------

excess <- readRDS(paths$input$excess)


# -----------------------------------------------------------------

dat <-
  excess %>%
  filter(timeframe == 'weekly') %>%
  group_by(model_id, region_iso, sex, age_group, cv_id) %>%
  mutate(
    covid_cases_2week_lag = lag(c19_cases_wkl, n = 3),
    covid_cases_3week_lag = lag(c19_cases_wkl, n = 4),
    covid_cases_log_delta =
      log(covid_cases_2week_lag)-log(covid_cases_3week_lag),
    excess_1week_lag = lag(xc2_wkl_q90, n = 1),
    excess_pct_delta =
      log(xc2_wkl_q90) - log(excess_1week_lag)
  ) %>%
  ungroup()

dat %>%
  filter(region_iso == 'DE', cv_id == 1) %>%
  filter(age_group %in% '[15,65)', sex == 'Male') %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  mutate(sigexcess = xc2_wkl_q90 > 0) %>%
  ggplot() +
  geom_vline(xintercept = as.Date(paste0(c(2020:2023), '-01-01'))) +
  aes(
    x = date
  ) +
  geom_col(aes(y = xc1_wkl_q50, fill = sigexcess), color = NA, just = 0) +
  geom_step(aes(y = c19_deaths_wkl), color = 'red', na.rm = TRUE) +
  scale_x_date(
    date_breaks = '1 month', date_labels = '%b'
  ) +
  facet_wrap(~model_id) +
  scale_fill_manual(values = c(`TRUE` = 'grey50', `FALSE` = 'grey70'))


# Export ----------------------------------------------------------

# export results of analysis
