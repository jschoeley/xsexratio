# Calculate excess deaths under various models

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
  config = read_yaml(path$config)
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
  readRDS(path$observed_and_expected) %>%
  unnest(predictions) %>%
  filter(cv_sample == 'test', model_id == 'MAV', cv_id == 0) %>%
  select(
    cv_id, cv_sample, region_iso, model_id, obs_id, sex, age_group,
    iso_year, iso_week, date,
    deaths_observed, personweeks, covid_cases, covid_deaths,
    temperature, temperature_anomaly,
    starts_with('deaths')
  )

# Add age standardized observed and simulated deaths --------------

excess$observed_and_expected <-
  observed_and_expected %>%
  left_join(
    tibble(
      age_group = unique(observed_and_expected$age_group),
      # european standard population
      stdpop = unlist(cnst$config$stdpop)
    )
  ) %>%
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
  as_tibble(excess$observed_and_expected_with_agesex_totals) %>%
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
  excess$observed_and_expected_with_agesex_totals %>%
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

# since 2020
excess$observed_and_expected_pandemic <-
  excess$observed_and_expected_weekly %>%
  group_by(cv_id, cv_sample, region_iso, model_id, age_group, sex) %>%
  summarise(
    iso_year = 2020, iso_week = 0, date = first(ISOWeekDateToDate(iso_year, 1)),
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup()

# annual
excess$observed_and_expected_annual <-
  excess$observed_and_expected_weekly %>%
  group_by(cv_id, cv_sample, region_iso, model_id, age_group, sex, iso_year) %>%
  summarise(
    iso_week = 0, date = first(ISOWeekDateToDate(iso_year, 1)),
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup()

# by quarter
excess$observed_and_expected_quarter <-
  excess$observed_and_expected_weekly %>%
  mutate(iso_week = case_when(
    iso_week %in% 1:12 ~ 1*12,
    iso_week %in% 13:24 ~ 2*12,
    iso_week %in% 25:36 ~ 3*12,
    iso_week %in% 37:48 ~ 4*12,
    TRUE ~ 5
  )) %>%
  group_by(cv_id, cv_sample, region_iso, model_id, age_group,
           sex, iso_year, iso_week) %>%
  summarise(
    date = ISOWeekDateToDate(iso_year, iso_week)[1],
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup()

# by month
excess$observed_and_expected_monthly <-
  excess$observed_and_expected_weekly %>%
  mutate(iso_week = case_when(
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
    iso_week %in% 49:53 ~ 13*4,
    TRUE ~ 5
  )) %>%
  group_by(cv_id, cv_sample, region_iso, model_id, age_group,
           sex, iso_year, iso_week) %>%
  summarise(
    date = ISOWeekDateToDate(iso_year, iso_week)[1],
    across(c(personweeks, deaths_predicted, deaths_observed,
             covid_cases, covid_deaths,
             matches('deaths_sim[[:digit:]]+$'),
             stdpop, deaths_observed_std, deaths_predicted_std,
             matches('deaths_sim[[:digit:]]+_std$')
    ), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup()

# combine
excess$observed_and_expected_combined <- bind_rows(
  pandemic = excess$observed_and_expected_pandemic,
  weekly = excess$observed_and_expected_weekly,
  annual = excess$observed_and_expected_annual,
  quarter = excess$observed_and_expected_quarter,
  monthly = excess$observed_and_expected_monthly,
  .id = 'timeframe'
)

# nest by model id and stratum
excess$observed_and_expected_nest <-
  excess$observed_and_expected_combined %>%
  nest(data = c(iso_year, iso_week, date, personweeks, stdpop,
                covid_cases, covid_deaths,
                starts_with('deaths')))

saveRDS(excess$observed_and_expected_nest, 'tmp/observed_and_expected_samples.rds')

# Calculate excess death statistics -------------------------------

# names of columns for expected/simulated deaths
excess$varnames_simdeath <-
  grep('deaths_sim[[:digit:]]+$', colnames(excess$observed_and_expected), value = TRUE)
excess$varnames_simdeath_std <-
  grep('deaths_sim[[:digit:]]+_std$', colnames(excess$observed_and_expected), value = TRUE)

# define quantiles
excess$quantiles1 <- c('q05' = 0.05, 'q25' = 0.25, 'q50' = 0.5, 'q75' = 0.75, 'q95' = 0.95)
excess$quantiles2 <- c('q50' = 0.5, 'q70' = 0.70, 'q90' = 0.9, 'q95' = 0.95, 'q99' = 0.99)

excess$excess_measures <-
  excess$observed_and_expected_nest %>%
  group_by(timeframe, cv_id, model_id, region_iso, age_group) %>%
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

    # weekly observed deaths
    obs_wkl_f <- X_f$deaths_observed
    obs_wkl_m <- X_m$deaths_observed
    obs_wkl_t <- X_t$deaths_observed
    # cumulative observed deaths (at end of week)
    obs_cum_f <- cumsum(obs_wkl_f)
    obs_cum_m <- cumsum(obs_wkl_m)
    obs_cum_t <- cumsum(obs_wkl_t)

    # weekly covid cases / deaths
    c19_cases_wkl_f <- X_f$covid_cases
    c19_cases_wkl_m <- X_m$covid_cases
    c19_cases_wkl_t <- X_t$covid_cases
    c19_deaths_wkl_f <- X_f$covid_deaths
    c19_deaths_wkl_m <- X_m$covid_deaths
    c19_deaths_wkl_t <- X_t$covid_deaths

    # simulated weekly expected deaths
    xpc_wkl_sim_f <- as.matrix(X_f[,excess$varnames_simdeath])
    xpc_wkl_sim_m <- as.matrix(X_m[,excess$varnames_simdeath])
    xpc_wkl_sim_t <- as.matrix(X_t[,excess$varnames_simdeath])
    # simulated cumulative expected deaths
    xpc_cum_sim_f <- SafeColCumSum(xpc_wkl_sim_f)
    xpc_cum_sim_m <- SafeColCumSum(xpc_wkl_sim_m)
    xpc_cum_sim_t <- SafeColCumSum(xpc_wkl_sim_t)

    # weekly expected deaths quantiles
    xpc_wkl_f <- Rowquantiles(xpc_wkl_sim_f, excess$quantiles1)
    xpc_wkl_m <- Rowquantiles(xpc_wkl_sim_m, excess$quantiles1)
    xpc_wkl_t <- Rowquantiles(xpc_wkl_sim_t, excess$quantiles1)
    colnames(xpc_wkl_f) <- paste0('xpc_wkl_f_', names(excess$quantiles1))
    colnames(xpc_wkl_m) <- paste0('xpc_wkl_m_,', names(excess$quantiles1))
    colnames(xpc_wkl_t) <- paste0('xpc_wkl_t_', names(excess$quantiles1))
    # cumulative expected deaths quantiles
    xpc_cum_f <- Rowquantiles(xpc_cum_sim_f, excess$quantiles1)
    xpc_cum_m <- Rowquantiles(xpc_cum_sim_m, excess$quantiles1)
    xpc_cum_t <- Rowquantiles(xpc_cum_sim_t, excess$quantiles1)
    colnames(xpc_cum_f) <- paste0('xpc_cum_f_', names(excess$quantiles1))
    colnames(xpc_cum_m) <- paste0('xpc_cum_m_', names(excess$quantiles1))
    colnames(xpc_cum_t) <- paste0('xpc_cum_t_', names(excess$quantiles1))

    CensorAtZero <- function (x) { ifelse(x<=0, 0, x) }

    # weekly excess deaths type 1 quantiles (negative excess censored at 0)
    xc1_wkl_f <- Rowquantiles(CensorAtZero(obs_wkl_f-xpc_wkl_sim_f), excess$quantiles1)
    xc1_wkl_m <- Rowquantiles(CensorAtZero(obs_wkl_m-xpc_wkl_sim_m), excess$quantiles1)
    xc1_wkl_t <- Rowquantiles(CensorAtZero(obs_wkl_t-xpc_wkl_sim_t), excess$quantiles1)
    colnames(xc1_wkl_f) <- paste0('xc1_wkl_f_', names(excess$quantiles1))
    colnames(xc1_wkl_m) <- paste0('xc1_wkl_m_', names(excess$quantiles1))
    colnames(xc1_wkl_t) <- paste0('xc1_wkl_t_', names(excess$quantiles1))
    # cumulative excess deaths type 1 quantiles (negative excess censored at 0)
    xc1_cum_f <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_wkl_f-xpc_wkl_sim_f)), excess$quantiles1)
    xc1_cum_m <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_wkl_m-xpc_wkl_sim_m)), excess$quantiles1)
    xc1_cum_t <- Rowquantiles(SafeColCumSum(CensorAtZero(obs_wkl_t-xpc_wkl_sim_t)), excess$quantiles1)
    colnames(xc1_cum_f) <- paste0('xc1_cum_f_', names(excess$quantiles1))
    colnames(xc1_cum_m) <- paste0('xc1_cum_m_', names(excess$quantiles1))
    colnames(xc1_cum_t) <- paste0('xc1_cum_t_', names(excess$quantiles1))

    # simulated weekly expected deaths 0 adjusted
    xpc_wkl_sim_zad_f <- xpc_wkl_sim_f
    xpc_wkl_sim_zad_m <- xpc_wkl_sim_m
    xpc_wkl_sim_zad_t <- xpc_wkl_sim_t
    xpc_wkl_sim_zad_f[xpc_wkl_sim_zad_f == 0] <- 1
    xpc_wkl_sim_zad_m[xpc_wkl_sim_zad_m == 0] <- 1
    xpc_wkl_sim_zad_t[xpc_wkl_sim_zad_t == 0] <- 1

    # weekly P-score quantiles
    psc_wkl_f <- Rowquantiles(
      CensorAtZero(obs_wkl_f-xpc_wkl_sim_f)/xpc_wkl_sim_zad_f*100,
      excess$quantiles1, type = 7
    )
    psc_wkl_m <- Rowquantiles(
      CensorAtZero(obs_wkl_m-xpc_wkl_sim_m)/xpc_wkl_sim_zad_m*100,
      excess$quantiles1, type = 7
    )
    psc_wkl_t <- Rowquantiles(
      CensorAtZero(obs_wkl_t-xpc_wkl_sim_t)/xpc_wkl_sim_zad_t*100,
      excess$quantiles1, type = 7
    )
    psc_wkl_f <- round(psc_wkl_f, digits = 2)
    psc_wkl_m <- round(psc_wkl_m, digits = 2)
    psc_wkl_t <- round(psc_wkl_t, digits = 2)
    colnames(psc_wkl_f) <- paste0('psc_wkl_f_', names(excess$quantiles1))
    colnames(psc_wkl_m) <- paste0('psc_wkl_m_', names(excess$quantiles1))
    colnames(psc_wkl_t) <- paste0('psc_wkl_t_', names(excess$quantiles1))
    # cumulative P-score quantiles
    psc_cum_f <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_wkl_f-xpc_wkl_sim_f))/SafeColCumSum(xpc_wkl_sim_zad_f)*100,
      excess$quantiles1, type = 7
    )
    psc_cum_m <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_wkl_m-xpc_wkl_sim_m))/SafeColCumSum(xpc_wkl_sim_zad_m)*100,
      excess$quantiles1, type = 7
    )
    psc_cum_t <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_wkl_t-xpc_wkl_sim_t))/SafeColCumSum(xpc_wkl_sim_zad_t)*100,
      excess$quantiles1, type = 7
    )
    psc_cum_f <- round(psc_cum_f, digits = 2)
    psc_cum_m <- round(psc_cum_m, digits = 2)
    psc_cum_t <- round(psc_cum_t, digits = 2)
    colnames(psc_cum_f) <- paste0('psc_cum_f_', names(excess$quantiles1))
    colnames(psc_cum_m) <- paste0('psc_cum_m_', names(excess$quantiles1))
    colnames(psc_cum_t) <- paste0('psc_cum_t_', names(excess$quantiles1))

    # sex difference in p-scores
    psc_wkl_sdf <- Rowquantiles(
      (CensorAtZero(obs_wkl_m-xpc_wkl_sim_m)/xpc_wkl_sim_zad_m*100)-
      (CensorAtZero(obs_wkl_f-xpc_wkl_sim_f)/xpc_wkl_sim_zad_f*100),
      excess$quantiles1, type = 7
    )
    psc_wkl_sdf <- round(psc_wkl_sdf, digits = 2)
    colnames(psc_wkl_sdf) <- paste0('psc_wkl_sdf_', names(excess$quantiles1))
    # sex difference in cumulative p-scores
    psc_cum_sdf <- Rowquantiles(
      (SafeColCumSum(CensorAtZero(obs_wkl_m-xpc_wkl_sim_m))/SafeColCumSum(xpc_wkl_sim_zad_m)*100)-
      (SafeColCumSum(CensorAtZero(obs_wkl_f-xpc_wkl_sim_f))/SafeColCumSum(xpc_wkl_sim_zad_f)*100),
      excess$quantiles1, type = 7
    )
    psc_cum_sdf <- round(psc_cum_sdf, digits = 2)
    colnames(psc_cum_sdf) <- paste0('psc_cum_sdf_', names(excess$quantiles1))

    # sex difference in excess deaths
    xc1_wkl_sdf <- Rowquantiles(
      CensorAtZero(obs_wkl_m-xpc_wkl_sim_m)-
      CensorAtZero(obs_wkl_m-xpc_wkl_sim_f),
      excess$quantiles1, type = 7
    )
    colnames(xc1_wkl_sdf) <- paste0('xc1_wkl_sdf_', names(excess$quantiles1))
    # sex difference in cumulative excess deaths
    xc1_cum_sdf <- Rowquantiles(
      SafeColCumSum(CensorAtZero(obs_wkl_m-xpc_wkl_sim_m))-
      SafeColCumSum(CensorAtZero(obs_wkl_m-xpc_wkl_sim_f)),
      excess$quantiles1, type = 7
    )
    colnames(xc1_cum_sdf) <- paste0('xc1_cum_sdf_', names(excess$quantiles1))

    timeseries_of_measures <-
      cbind(
        X_f[,c('iso_year', 'iso_week', 'personweeks')],
        obs_wkl_f, obs_wkl_m, obs_wkl_t,
        obs_cum_f, obs_cum_m, obs_cum_t,
        c19_cases_wkl_f, c19_cases_wkl_m, c19_cases_wkl_t,
        c19_deaths_wkl_f, c19_deaths_wkl_m, c19_deaths_wkl_t,
        xpc_wkl_f, xpc_wkl_m, xpc_wkl_t,
        xpc_cum_f, xpc_cum_m, xpc_cum_t,
        xc1_wkl_f, xc1_wkl_m, xc1_wkl_t,
        xc1_cum_f, xc1_cum_m, xc1_cum_t,
        psc_wkl_f, psc_wkl_m, psc_wkl_t,
        psc_cum_f, psc_cum_m, psc_cum_t,
        psc_wkl_sdf, psc_cum_sdf,
        xc1_wkl_sdf, xc1_cum_sdf
      )

    return(timeseries_of_measures)

  }) %>%
  ungroup()

# Export ----------------------------------------------------------

saveRDS(excess$excess_measures, path$excess_deaths)
