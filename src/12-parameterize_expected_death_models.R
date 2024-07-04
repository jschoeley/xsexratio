# Parameterize expected death models
#
# A list specifying all the models to be tested, and their
# parametrizations. See specify_models.R for the exact
# implementation of the models.

# Init ------------------------------------------------------------

library(dplyr)
library(here)

wd <- here(); setwd(wd)

path <- list(
  mod_para = 'tmp/mod_para.rds'
)

# Specifications of models to test --------------------------------

# Just a big list specifying all the models to be tested,
# and their parametrizations. See specify_models.R for the exact
# implementation of the models.

mod_para <-
  tribble(

    ~model_id, ~model_class, ~model_para,

    # 5 year average death rates
    'AVR5y', 'gam', list(
      formula = formula(
        deaths_observed ~
          as.factor(iso_week) +
          offset(log(personweeks))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),

    # 5 year average death counts
    'AVC5y', 'gam', list(
      formula = formula(
        deaths_observed ~
          as.factor(iso_week)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),

    # Serfling style regression
    # Forecasting Serfling with exposures
    # AIC selection of seasonality
    'SRF7y', 'glm', list(
      models = list(
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # half year period
            sin(2*pi/26*iso_week) +
            cos(2*pi/26*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # exposures
            offset(log(personweeks))
        )
      ),
      family = quasipoisson(link = 'log'),
      weeks_for_training = NULL,
      n_years_for_training = 7
    ),

    # Serfling style regression
    # Forecasting Serfling with exposures
    # AIC selection of seasonality
    'SRF5y', 'glm', list(
      models = list(
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # half year period
            sin(2*pi/26*iso_week) +
            cos(2*pi/26*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # exposures
            offset(log(personweeks))
        )
      ),
      family = quasipoisson(link = 'log'),
      weeks_for_training = NULL,
      n_years_for_training = 5
    ),

    # Count GAM
    'GAM7yt', 'gam', list(
      formula = formula(
        deaths_observed ~
          # log linear long term trend
          origin_weeks +
          # penalized cyclic spline for seasonality
          s(epi_week, bs = 'cp', k = 12, fx = FALSE) +
          # temperature effect
          s(epi_week, bs = 'cp', k = 12, fx = FALSE, by = temperature_anomaly) +
          # adjustment for special weeks
          s(holiday, bs = 're') +
          # exposures
          offset(log(personweeks))
      ),
      family = mgcv::nb(link = 'log'),
      n_years_for_training = 7
    ),

    'GAM7y', 'gam', list(
      formula = formula(
        deaths_observed ~
          # log linear long term trend
          origin_weeks +
          # penalized cyclic spline for seasonality
          s(epi_week, bs = 'cp', k = 12, fx = FALSE) +
          # temperature effect
          #s(epi_week, bs = 'cp', k = 12, fx = FALSE, by = temperature_anomaly) +
          # adjustment for special weeks
          #s(holiday, bs = 're') +
          # exposures
          offset(log(personweeks))
      ),
      family = mgcv::nb(link = 'log'),
      n_years_for_training = 7
    ),

    # Kontis' latent Gaussian autoregressive seasonal Poisson regression
    'LGM7y', 'lgm', list(
      formula = formula(
        death ~
          1 +
          global_slope +
          holiday +
          f(time_ar,
            model = 'ar1',
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          f(time_seas,
            model = 'seasonal', season.length = 52,
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          # independent remaining errors
          f(resid_iid,
            model = 'iid',
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          offset(log(exposure))
      ),
      # simpler formula for low sample size strata
      formula_alt = formula(
          death ~
            1 +
            global_slope +
            f(time_ar,
              model = 'ar1',
              hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
            ) +
            # independent remaining errors
            f(resid_iid,
              model = 'iid',
              hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
            ) +
            offset(log(exposure))
        ),
      weeks_for_training_within_year = NULL,
      weeks_for_training_pre_test = 7*52
    ),

    # Exponential smoothing
    "ESM7y", "adam", list(
      lags = c(1, 1, 52)
    ),

    # WHO GAM
    # https://cdn.who.int/media/docs/default-source/world-health-data-platform/covid-19-excessmortality/who_methods_for_estimating_the_excess_mortality_associated_with_the_covid-19_pandemic.pdf?sfvrsn=5a05fa76_1&download=true
    'WHO5y', 'gam', list(
      formula = formula(
        deaths_observed ~
          s(iso_year, bs = 'tp', k = 3) +
          # penalized cyclic spline for seasonality
          s(epi_week, bs = 'cp', fx = FALSE)
      ),
      family = mgcv::nb(link = 'log'),
      n_years_for_training = 5
    ),

    # K&K
    'KUK7y', 'gam', list(
      formula = formula(
        deaths_observed ~
          # log linear long term trend
          iso_year +
          # fixed week effects
          epi_week_fac
      ),
      family = mgcv::nb(link = 'log'),
      n_years_for_training = 7
    )

  )

# Export ----------------------------------------------------------

saveRDS(mod_para, file = path$mod_para)
