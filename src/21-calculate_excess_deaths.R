# Calculate excess deaths statistics
#
# pyr_int_f, pyr_int_m, pyr_int_t:
#   Person-years exposure over current interval
#   (male, female, total)
# pyr_cum_f, pyr_cum_m, pyr_cum_t:
#   Person-years exposure over cumulative intervals
# obs_int_f, obs_int_m, obs_int_t:
#   Observed deaths over current interval
# obs_cum_f, obs_cum_m, obs_cum_t:
#   Observed deaths over cumulative intervals
# c19_cases_int_f, c19_cases_int_m, c19_cases_int_t:
#   Covid-19 incidence counts over current interval
# c19_deaths_int_f, c19_deaths_int_m, c19_deaths_int_t:
#   Covid-19 incidence counts over cumulative intervals
# xpc_int_f, xpc_int_m, xpc_int_t:
#   Expected deaths over current interval
# xpc_cum_f, xpc_cum_m, xpc_cum_t:
#   Expected deaths over cumulative intervals
# xc1_int_f, xc1_int_m, xc1_int_t:
#   Excess deaths (truncated at 0) over current interval
# xc1_cum_f, xc1_cum_m, xc1_cum_t:
#   Excess deaths (truncated at 0) over cumulative intervals
# xr1_int_f, xr1_int_m, xr1_int_t:
#   Excess death rate (truncated at 0) over current interval
# xr1_cum_f, xr1_cum_m, xr1_cum_t:
#   Excess death rate (truncated at 0) over cumulative intervals
# psc_int_f, psc_int_m, psc_int_t:
#   Excess death P-score (truncated at 0) over current interval
# psc_cum_f, psc_cum_m, psc_cum_t:
#   Excess death P-score (truncated at 0) over cumulative interval
# psc_int_sdf, psc_cum_sdf:
#   Sex differences (male - female) in P-scores over current/cumulative intervals
# xc1_int_sdf, xc1_cum_sdf:
#   Sex differences (male - female) in excess deaths over current/cumulative intervals
# xr1_int_sdf, xr1_cum_sdf:
#   Sex differences (male - female) in excess death rates over current/cumulative intervals

# Init ------------------------------------------------------------

library(here)
library(tidyverse)
library(yaml)

wd <- here(); setwd(wd)

# paths
path <- list(
  # global objects
  glob = 'src/00-global_objects.R',
  # observed and expected death counts 2020 and later
  observed_and_expected = 'tmp/expected_deaths_cv.rds',
  # excess deaths
  excess_deaths = 'tmp/excess_deaths.rds',
  out = 'out',
  config = 'src/config.yaml'
)

cnst <- list(
  config = read_yaml(path$config),
  analysis_start = '2020-01-27',
  analysis_end = '2023-12-25'
)

# global functions and constants
source(path$glob)

excess <- list()

# Functions -------------------------------------------------------

SafeColCumSum <- function (X) {
  N <- nrow(X)
  Z <- apply(X, 2, cumsum)
  if (N == 1) {
    Z <- matrix(Z, nrow = 1)
    colnames(Z) <- colnames(X)
  }
  return(Z)
}

# Load data -------------------------------------------------------

# load data on predicted death counts
observed_and_expected <-
  readRDS(path$observed_and_expected) |>
  unnest(predictions) |>
  filter(cv_sample == 'test', model_id == 'MAV', cv_id == 0) |>
  select(
    cv_id, cv_sample, region_iso, model_id, obs_id, sex, age_group,
    iso_year, iso_week, date,
    deaths_observed, personweeks, covid_cases, covid_deaths,
    temperature, temperature_anomaly,
    starts_with('deaths')
  )

# Add age standardized observed and simulated deaths --------------

excess$observed_and_expected <-
  observed_and_expected |>
  left_join(
    tibble(
      age_group = unique(observed_and_expected$age_group),
      # european standard population
      stdpop = unlist(cnst$config$stdpop)
    )
  ) |>
  # direct standardization of observed and expected deaths
  # with european standard population
  mutate(across(
    c(deaths_observed, deaths_predicted, starts_with('deaths_sim')),
    ~{floor(.x/personweeks*stdpop)}, .names = "{.col}_std"
  ))

# Aggregate totals by age and sex ---------------------------------

library(data.table)

excess$observed_and_expected_dt <-
  as.data.table(excess$observed_and_expected)

excess$strata_cols <-
  c('cv_id', 'cv_sample', 'model_id', 'region_iso', 'sex',
    'age_group', 'iso_year', 'iso_week', 'date')
excess$value_cols <-
  c(
    'stdpop', 'personweeks',
    grep('deaths|covid',
         names(excess$observed_and_expected), value = TRUE)
  )

excess$observed_and_expected_with_agesex_totals <-
  groupingsets(
    excess$observed_and_expected_dt,
    j = lapply(.SD, sum),
    by = excess$strata_cols,
    sets = list(
      excess$strata_cols,
      excess$strata_cols[-which(excess$strata_cols %in% 'sex')],
      excess$strata_cols[-which(excess$strata_cols %in% 'age_group')],
      excess$strata_cols[-which(excess$strata_cols %in% c('sex', 'age_group'))]
    ),
    .SDcols = excess$value_cols
  )
excess$observed_and_expected_with_agesex_totals <-
  as_tibble(excess$observed_and_expected_with_agesex_totals) |>
  mutate(
    age_group = as.character(age_group),
    age_group = ifelse(is.na(age_group), 'Total', age_group),
    sex = ifelse(is.na(sex), 'Total', sex)
  )

# Account for missing data ----------------------------------------

# if deaths are unknown for a given week, then set other observations
# to NA as well. this allows to aggregate and analyse death counts
# and rates over longer time periods while ignoring the part of the
# period which is unobserved.
excess$observed_and_expected_weekly <-
  excess$observed_and_expected_with_agesex_totals |>
  mutate(across(
    c(
      personweeks, stdpop,
      covid_cases, covid_deaths,
      deaths_predicted,
      deaths_observed_std, deaths_predicted_std,
      starts_with('deaths_sim')
    ),
    ~{ifelse(is.na(deaths_observed), NA, .x)}
  ))

# Aggregate over various timeframes -------------------------------

# add the variables <timeframe>, e.g. 'quarterly', and
# <timeframe_value>, e.g. '2021-Q1' and aggregate counts
# accordingly.

# weekly 2020w5 through 2023w52
excess$observed_and_expected_weekly <-
  excess$observed_and_expected_weekly |>
  filter(date >= cnst$analysis_start, date <= cnst$analysis_end) |>
  mutate(timeframe_value = paste(iso_year, formatC(iso_week, flag = '0', width = 2),
                                 sep = '-')) |>
  select(cv_id, cv_sample, region_iso, model_id, age_group, sex,
         timeframe_value, everything())

# total 2020w5 through 2023w52
excess$observed_and_expected_pandemic <-
  excess$observed_and_expected_weekly |>
  filter(date >= cnst$analysis_start, date <= cnst$analysis_end) |>
  mutate(timeframe_value = '2020w01-2023w52') |>
  group_by(cv_id, cv_sample, region_iso, model_id, age_group, sex,
           timeframe_value) |>
  summarise(
    iso_year = year(cnst$analysis_start),
    iso_week = isoweek(cnst$analysis_start),
    date = lubridate::as_date(cnst$analysis_start),
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) |>
  ungroup()

# epi year
excess$observed_and_expected_epiyear <-
  excess$observed_and_expected_weekly |>
  filter(date >= cnst$analysis_start, date <= cnst$analysis_end) |>
  mutate(
    timeframe_value = case_when(
      iso_year == 2020 & iso_week >= 4 & iso_week < 27 ~
        '2020w05-w26',
      (iso_year == 2020 & iso_week >= 27) | (iso_year == 2021 & iso_week < 27) ~
        '2020w27-21w26',
      (iso_year == 2021 & iso_week >= 27) | (iso_year == 2022 & iso_week < 27) ~
        '2021w27-22w26',
      (iso_year == 2022 & iso_week >= 27) | (iso_year == 2023 & iso_week < 27) ~
        '2022w27-23w26',
      iso_year == 2023 & iso_week >= 27 ~
        '2023w27-w52'
    ),
    iso_year = case_when(
      timeframe_value == '2020w05-w26'  ~ 2020,
      timeframe_value == '2020w27-21w26' ~ 2020,
      timeframe_value == '2021w27-22w26' ~ 2021,
      timeframe_value == '2022w27-23w26' ~ 2022,
      timeframe_value == '2023w27-w52' ~ 2023
    ),
    iso_week = case_when(
      timeframe_value == '2020w05-w26'  ~ 5,
      timeframe_value == '2020w27-21w26' ~ 27,
      timeframe_value == '2021w27-22w26' ~ 27,
      timeframe_value == '2022w27-23w26' ~ 27,
      timeframe_value == '2023w27-w52' ~ 27
    )
  ) |>
  group_by(cv_id, cv_sample, region_iso, model_id, age_group,
           sex, timeframe_value) |>
  summarise(
    iso_year = first(iso_year),
    iso_week = first(iso_week),
    date = ISOWeekDateToDate(iso_year, iso_week),
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) |>
  ungroup()

# by month
excess$observed_and_expected_monthly <-
  excess$observed_and_expected_weekly |>
  filter(date >= cnst$analysis_start, date <= cnst$analysis_end) |>
  mutate(
    iso_week = case_when(
      iso_week %in% 1:4   ~ 1*4,
      iso_week %in% 5:8   ~ 2*4,
      iso_week %in% 9:12  ~ 3*4,
      iso_week %in% 13:16 ~ 4*4,
      iso_week %in% 17:20 ~ 5*4,
      iso_week %in% 21:24 ~ 6*4,
      iso_week %in% 25:28 ~ 7*4,
      iso_week %in% 29:32 ~ 8*4,
      iso_week %in% 33:36 ~ 9*4,
      iso_week %in% 37:40 ~ 10*4,
      iso_week %in% 41:44 ~ 11*4,
      iso_week %in% 45:48 ~ 12*4,
      iso_week %in% 49:53 ~ 13*4
    ),
    timeframe_value = paste(iso_year, formatC(iso_week, flag = '0', width = 2), sep = '-'),
  ) |>
  group_by(cv_id, cv_sample, region_iso, model_id, age_group,
           sex, timeframe_value) |>
  summarise(
    iso_year = as.numeric(substr(first(timeframe_value), 1, 4)),
    iso_week = as.numeric(substr(first(timeframe_value), 6, 7)),
    date = ISOWeekDateToDate(iso_year, iso_week),
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) |>
  ungroup()

# combine
excess$observed_and_expected_combined <- bind_rows(
  pandemic = excess$observed_and_expected_pandemic,
  weekly = excess$observed_and_expected_weekly,
  epiyear = excess$observed_and_expected_epiyear,
  monthly = excess$observed_and_expected_monthly,
  .id = 'timeframe'
)

# nest by model id and stratum
excess$observed_and_expected_nest <-
  excess$observed_and_expected_combined |>
  nest(data = c(timeframe_value, iso_year, iso_week, date, personweeks, stdpop,
                covid_cases, covid_deaths,
                starts_with('deaths')))

saveRDS(excess$observed_and_expected_nest,
        'tmp/observed_and_expected_samples.rds')

# Calculate excess death statistics -------------------------------

# names of columns for expected/simulated deaths
excess$varnames_simdeath <-
  grep('deaths_sim[[:digit:]]+$',
       colnames(excess$observed_and_expected), value = TRUE)
excess$varnames_simdeath_std <-
  grep('deaths_sim[[:digit:]]+_std$',
       colnames(excess$observed_and_expected), value = TRUE)

# define quantiles
excess$quantiles1 <- c('q025' = 0.025, 'q05' = 0.05, 'q25' = 0.25,
                       'q50' = 0.5, 'q75' = 0.75, 'q95' = 0.95, 'q975' = 0.975)
excess$quantiles2 <- c('q50' = 0.5, 'q70' = 0.70, 'q90' = 0.9, 'q95' = 0.95, 'q99' = 0.99)

excess$excess_measures <-
  excess$observed_and_expected_nest |>
  group_by(timeframe, cv_id, model_id, region_iso, age_group) |>
  group_modify(~{

    cat(.y[['timeframe']], .y[['cv_id']], .y[['model_id']],
        .y[['region_iso']], .y[['sex']],
        as.character(.y[['age_group']]), '\n')

    # are we looking at all ages?
    allage <- .y[['age_group']] == 'Total'

    # a time ordered data frame of observed and simulated death counts
    # for a single region, sex, age group and model, starting early 2020
    # 1 row per observation
    X_f <- .x[.x$sex==cnst$config$skeleton$sex$Female,]$data[[1]]
    X_m <- .x[.x$sex==cnst$config$skeleton$sex$Male,]$data[[1]]
    X_t <- .x[.x$sex=='Total',]$data[[1]]

    # return data frame of row-wise quantiles over columns of X
    Rowquantiles <- function (X, prob, type = 4, na.rm = TRUE) {
      t(apply(X, 1, quantile, prob = prob, type = type, na.rm = na.rm))
    }

    # weekly person-years exposure
    pyr_int_f <- X_f$personweeks/52
    pyr_int_m <- X_m$personweeks/52
    pyr_int_t <- X_t$personweeks/52
    # cumulative person-years exposure (at end of week)
    pyr_cum_f <- cumsum(pyr_int_f)
    pyr_cum_m <- cumsum(pyr_int_m)
    pyr_cum_t <- cumsum(pyr_int_t)

    # weekly observed deaths
    obs_int_f <- X_f$deaths_observed
    obs_int_m <- X_m$deaths_observed
    obs_int_t <- X_t$deaths_observed
    # cumulative observed deaths (at end of week)
    obs_cum_f <- cumsum(obs_int_f)
    obs_cum_m <- cumsum(obs_int_m)
    obs_cum_t <- cumsum(obs_int_t)

    # weekly covid cases / deaths
    c19_cases_int_f <- X_f$covid_cases
    c19_cases_int_m <- X_m$covid_cases
    c19_cases_int_t <- X_t$covid_cases
    c19_deaths_int_f <- X_f$covid_deaths
    c19_deaths_int_m <- X_m$covid_deaths
    c19_deaths_int_t <- X_t$covid_deaths

    # simulated weekly expected deaths
    xpc_int_sim_f <- as.matrix(X_f[,excess$varnames_simdeath])
    xpc_int_sim_m <- as.matrix(X_m[,excess$varnames_simdeath])
    xpc_int_sim_t <- as.matrix(X_t[,excess$varnames_simdeath])
    # simulated cumulative expected deaths
    xpc_cum_sim_f <- SafeColCumSum(xpc_int_sim_f)
    xpc_cum_sim_m <- SafeColCumSum(xpc_int_sim_m)
    xpc_cum_sim_t <- SafeColCumSum(xpc_int_sim_t)

    # weekly expected deaths quantiles
    xpc_int_f <- Rowquantiles(xpc_int_sim_f, excess$quantiles1)
    xpc_int_m <- Rowquantiles(xpc_int_sim_m, excess$quantiles1)
    xpc_int_t <- Rowquantiles(xpc_int_sim_t, excess$quantiles1)
    colnames(xpc_int_f) <- paste0('xpc_int_f_', names(excess$quantiles1))
    colnames(xpc_int_m) <- paste0('xpc_int_m_', names(excess$quantiles1))
    colnames(xpc_int_t) <- paste0('xpc_int_t_', names(excess$quantiles1))
    # cumulative expected deaths quantiles
    xpc_cum_f <- Rowquantiles(xpc_cum_sim_f, excess$quantiles1)
    xpc_cum_m <- Rowquantiles(xpc_cum_sim_m, excess$quantiles1)
    xpc_cum_t <- Rowquantiles(xpc_cum_sim_t, excess$quantiles1)
    colnames(xpc_cum_f) <- paste0('xpc_cum_f_', names(excess$quantiles1))
    colnames(xpc_cum_m) <- paste0('xpc_cum_m_', names(excess$quantiles1))
    colnames(xpc_cum_t) <- paste0('xpc_cum_t_', names(excess$quantiles1))

    CensorAtZero <- function (x) { ifelse(x<=0, 0, x) }

    # weekly excess deaths type 1 quantiles (negative excess censored at 0)
    xc1_int_f <- Rowquantiles(CensorAtZero(obs_int_f-xpc_int_sim_f), excess$quantiles1)
    xc1_int_m <- Rowquantiles(CensorAtZero(obs_int_m-xpc_int_sim_m), excess$quantiles1)
    xc1_int_t <- Rowquantiles(CensorAtZero(obs_int_t-xpc_int_sim_t), excess$quantiles1)
    colnames(xc1_int_f) <- paste0('xc1_int_f_', names(excess$quantiles1))
    colnames(xc1_int_m) <- paste0('xc1_int_m_', names(excess$quantiles1))
    colnames(xc1_int_t) <- paste0('xc1_int_t_', names(excess$quantiles1))
    # cumulative excess deaths type 1 quantiles (negative excess censored at 0)
    xc1_cum_f <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f)), excess$quantiles1)
    xc1_cum_m <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m)), excess$quantiles1)
    xc1_cum_t <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_t-xpc_int_sim_t)), excess$quantiles1)
    colnames(xc1_cum_f) <- paste0('xc1_cum_f_', names(excess$quantiles1))
    colnames(xc1_cum_m) <- paste0('xc1_cum_m_', names(excess$quantiles1))
    colnames(xc1_cum_t) <- paste0('xc1_cum_t_', names(excess$quantiles1))

    # weekly excess deaths type 1 rate quantiles (negative excess censored at 0)
    xr1_int_f <- Rowquantiles(CensorAtZero(obs_int_f-xpc_int_sim_f)/pyr_int_f, excess$quantiles1)
    xr1_int_m <- Rowquantiles(CensorAtZero(obs_int_m-xpc_int_sim_m)/pyr_int_m, excess$quantiles1)
    xr1_int_t <- Rowquantiles(CensorAtZero(obs_int_t-xpc_int_sim_t)/pyr_int_t, excess$quantiles1)
    colnames(xr1_int_f) <- paste0('xr1_int_f_', names(excess$quantiles1))
    colnames(xr1_int_m) <- paste0('xr1_int_m_', names(excess$quantiles1))
    colnames(xr1_int_t) <- paste0('xr1_int_t_', names(excess$quantiles1))
    # cumulative excess deaths type 1 quantiles (negative excess censored at 0)
    xr1_cum_f <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f))/pyr_cum_f, excess$quantiles1)
    xr1_cum_m <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m))/pyr_cum_m, excess$quantiles1)
    xr1_cum_t <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_int_t-xpc_int_sim_t))/pyr_cum_t, excess$quantiles1)
    colnames(xr1_cum_f) <- paste0('xr1_cum_f_', names(excess$quantiles1))
    colnames(xr1_cum_m) <- paste0('xr1_cum_m_', names(excess$quantiles1))
    colnames(xr1_cum_t) <- paste0('xr1_cum_t_', names(excess$quantiles1))


    # simulated weekly expected deaths 0 adjusted
    xpc_int_sim_zad_f <- xpc_int_sim_f
    xpc_int_sim_zad_m <- xpc_int_sim_m
    xpc_int_sim_zad_t <- xpc_int_sim_t
    xpc_int_sim_zad_f[xpc_int_sim_zad_f == 0] <- 1
    xpc_int_sim_zad_m[xpc_int_sim_zad_m == 0] <- 1
    xpc_int_sim_zad_t[xpc_int_sim_zad_t == 0] <- 1

    # weekly P-score quantiles
    psc_int_f <- Rowquantiles(
      CensorAtZero(obs_int_f-xpc_int_sim_f)/xpc_int_sim_zad_f*100,
      excess$quantiles1, type = 1
    )
    psc_int_m <- Rowquantiles(
      CensorAtZero(obs_int_m-xpc_int_sim_m)/xpc_int_sim_zad_m*100,
      excess$quantiles1, type = 1
    )
    psc_int_t <- Rowquantiles(
      CensorAtZero(obs_int_t-xpc_int_sim_t)/xpc_int_sim_zad_t*100,
      excess$quantiles1, type = 1
    )
    psc_int_f <- round(psc_int_f, digits = 2)
    psc_int_m <- round(psc_int_m, digits = 2)
    psc_int_t <- round(psc_int_t, digits = 2)
    colnames(psc_int_f) <- paste0('psc_int_f_', names(excess$quantiles1))
    colnames(psc_int_m) <- paste0('psc_int_m_', names(excess$quantiles1))
    colnames(psc_int_t) <- paste0('psc_int_t_', names(excess$quantiles1))
    # cumulative P-score quantiles
    psc_cum_f <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f))/SafeColCumSum(xpc_int_sim_zad_f)*100,
      excess$quantiles1, type = 1
    )
    psc_cum_m <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m))/SafeColCumSum(xpc_int_sim_zad_m)*100,
      excess$quantiles1, type = 1
    )
    psc_cum_t <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_int_t-xpc_int_sim_t))/SafeColCumSum(xpc_int_sim_zad_t)*100,
      excess$quantiles1, type = 1
    )
    psc_cum_f <- round(psc_cum_f, digits = 2)
    psc_cum_m <- round(psc_cum_m, digits = 2)
    psc_cum_t <- round(psc_cum_t, digits = 2)
    colnames(psc_cum_f) <- paste0('psc_cum_f_', names(excess$quantiles1))
    colnames(psc_cum_m) <- paste0('psc_cum_m_', names(excess$quantiles1))
    colnames(psc_cum_t) <- paste0('psc_cum_t_', names(excess$quantiles1))

    # sex difference in p-scores
    psc_int_sdf <- Rowquantiles(
      (CensorAtZero(obs_int_m-xpc_int_sim_m)/xpc_int_sim_zad_m*100)-
      (CensorAtZero(obs_int_f-xpc_int_sim_f)/xpc_int_sim_zad_f*100),
      excess$quantiles1, type = 1
    )
    psc_int_sdf <- round(psc_int_sdf, digits = 2)
    colnames(psc_int_sdf) <- paste0('psc_int_sdf_', names(excess$quantiles1))
    # sex difference in cumulative p-scores
    psc_cum_sdf <- Rowquantiles(
      (SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m))/SafeColCumSum(xpc_int_sim_zad_m)*100)-
      (SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f))/SafeColCumSum(xpc_int_sim_zad_f)*100),
      excess$quantiles1, type = 1
    )
    psc_cum_sdf <- round(psc_cum_sdf, digits = 2)
    colnames(psc_cum_sdf) <- paste0('psc_cum_sdf_', names(excess$quantiles1))

    # sex difference in excess deaths
    xc1_int_sdf <- Rowquantiles(
      CensorAtZero(obs_int_m-xpc_int_sim_m)-
      CensorAtZero(obs_int_f-xpc_int_sim_f),
      excess$quantiles1, type = 1
    )
    colnames(xc1_int_sdf) <- paste0('xc1_int_sdf_', names(excess$quantiles1))
    # sex difference in cumulative excess deaths
    xc1_cum_sdf <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m))-
      SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f)),
      excess$quantiles1, type = 1
    )
    colnames(xc1_cum_sdf) <- paste0('xc1_cum_sdf_', names(excess$quantiles1))

    # sex difference in excess death rates
    xr1_int_sdf <- Rowquantiles(
      # male excess death rate
      CensorAtZero(obs_int_m-xpc_int_sim_m)/pyr_int_m-
      # female excess death rate
      CensorAtZero(obs_int_f-xpc_int_sim_f)/pyr_int_f,
      excess$quantiles1, type = 1
    )
    colnames(xr1_int_sdf) <- paste0('xr1_int_sdf_', names(excess$quantiles1))
    # sex difference in cumulative excess death rates
    xr1_cum_sdf <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_int_m-xpc_int_sim_m))/pyr_cum_m-
      SafeColCumSum(CensorAtZero(obs_int_f-xpc_int_sim_f))/pyr_cum_f,
      excess$quantiles1, type = 1
    )
    colnames(xr1_cum_sdf) <- paste0('xr1_cum_sdf_', names(excess$quantiles1))


    timeseries_of_measures <-
      cbind(
        X_f[,c('timeframe_value', 'iso_year', 'iso_week')],
        pyr_int_f, pyr_int_m, pyr_int_t,
        pyr_cum_f, pyr_cum_m, pyr_cum_t,
        obs_int_f, obs_int_m, obs_int_t,
        obs_cum_f, obs_cum_m, obs_cum_t,
        #c19_cases_int_f, c19_cases_int_m, c19_cases_int_t,
        #c19_deaths_int_f, c19_deaths_int_m, c19_deaths_int_t,
        xpc_int_f, xpc_int_m, xpc_int_t,
        xpc_cum_f, xpc_cum_m, xpc_cum_t,
        xc1_int_f, xc1_int_m, xc1_int_t,
        xc1_cum_f, xc1_cum_m, xc1_cum_t,
        xr1_int_f, xr1_int_m, xr1_int_t,
        xr1_cum_f, xr1_cum_m, xr1_cum_t,
        psc_int_f, psc_int_m, psc_int_t,
        psc_cum_f, psc_cum_m, psc_cum_t,
        psc_int_sdf, psc_cum_sdf,
        xc1_int_sdf, xc1_cum_sdf,
        xr1_int_sdf, xr1_cum_sdf
      )

    return(timeseries_of_measures)

  }) |>
  ungroup()

# Export ----------------------------------------------------------

saveRDS(excess$excess_measures, path$excess_deaths)
