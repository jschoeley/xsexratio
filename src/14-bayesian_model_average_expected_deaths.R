# Perform bayesian model averaging on expected deaths

# Init ------------------------------------------------------------

set.seed(1987)

library(here); library(glue)
library(yaml)
library(tidyverse)

# Constants -------------------------------------------------------

wd <- here(); setwd(wd)

# paths
path <- list(
  config = 'src/config.yaml',
  expected_batch = 'tmp/expected/',
  glob = 'src/00-global_objects.R',
  expected_batch_files = list.files('tmp/expected/'),
  expected_deaths_cv = 'tmp/expected_deaths_cv.rds'
)

# global functions and constants
source(path$glob)

# constants
cnst <- list()
cnst <- within(cnst,{
  config = read_yaml(path$config)
  ndraws = 250
})

dat <- list()
fig <- list()

# Load data -------------------------------------------------------

# load data for cross validation
dat$expected <- map(
  paste0(path$expected_batch, path$expected_batch_files),
  ~readRDS(.x)
) %>% bind_rows()

# Perform model averaging -----------------------------------------

# for each observation sample N posterior predictions
# uniformly from the predictions of the various models
dat$fitted_models_with_mav <-
  dat$expected %>%
  mutate(
    weight = case_when(
      region_iso == 'CH' & model_id %in% c('AVC5y', 'AVR5y') ~ 0,
      region_iso == 'CH' ~ 1/7,
      region_iso == 'EE' & model_id %in% c('AVC5y', 'AVR5y', 'KUK7y', 'LGM7y') ~ 0,
      region_iso == 'EE' ~ 1/5,
      region_iso == 'IS' & model_id %in% c('AVC5y', 'AVR5y', 'KUK7y') ~ 0,
      region_iso == 'IS' ~ 1/6,
      region_iso == 'LU' & model_id %in% c('AVC5y', 'AVR5y', 'KUK7y', 'LGM7y') ~ 0,
      region_iso == 'LU' ~ 1/5,
      region_iso == 'LV' & model_id %in% c('AVC5y', 'AVR5y') ~ 0,
      region_iso == 'LV' ~ 1/7,
      region_iso == 'NO' & model_id %in% c('AVC5y', 'AVR5y') ~ 0,
      region_iso == 'NO' ~ 1/7,
      region_iso == 'PT' & model_id %in% c('AVC5y', 'AVR5y') ~ 0,
      region_iso == 'PT' ~ 1/7,
      region_iso == 'SI' & model_id %in% c('AVC5y', 'AVR5y') ~ 0,
      region_iso == 'SI' ~ 1/7,
      TRUE ~  1/length(cnst$config$models)
    )
  ) %>%
  #left_join(
  #  cnst$model_metadata %>% select(code, weight),
  #  by = c("model_id" = "code")
  #) %>%
  select(-data) %>%
  group_by(cv_id, region_iso) %>%
  group_modify(~ {
    cv_id <- .y[["cv_id"]]
    obs_id <- .x[1, ][["predictions"]][[1]][["obs_id"]]
    n_rows <- nrow(.x[1, ][["predictions"]][[1]])
    n_sim <- cnst$ndraws
    unique_models <- unique(.x$model_id)
    n_models <- length(unique_models)
    predictions_by_model <-
      array(
        dim = c(n_rows, n_sim + 1, n_models),
        dimnames = list(
          "obs_id" = obs_id,
          "sim_id" = 0:n_sim,
          "model_id" = unique_models
        )
      )
    for (k in unique_models) {
      cat(cv_id, k, "\n")
      predicted_and_simulated <-
        as.matrix(.x[.x$model_id == k, ][["predictions"]][[1]][, c("deaths_predicted", paste0("deaths_sim", 1:n_sim))])
      predictions_by_model[, , k] <- c(predicted_and_simulated)
    }
    # random index matrix selecting for each prediction
    I <- matrix(
      sample(
        1:n_models, n_rows*(n_sim+1), replace = TRUE,
        prob = .x$weight
      ),
      n_rows, (n_sim+1)
    )
    # weights sampled from dirichlet distribution
    # I <- matrix(NA, n_rows, (n_sim + 1))
    # for (j in 2:(n_sim + 1)) {
    #   w <- c(MCMCpack::rdirichlet(1, .x$weight))
    #   I[, j] <- sample(
    #     1:n_models, n_rows,
    #     replace = TRUE,
    #     prob = w
    #   )
    # }

    # https://stackoverflow.com/a/39344780
    predictions_modelaveraged <- matrix(
      predictions_by_model[cbind(
        rep(1:n_rows, n_sim + 1),
        rep(1:(n_sim + 1), each = n_rows),
        c(I)
      )],
      n_rows, n_sim + 1
    )
    prediction_names <- c("deaths_predicted", paste0("deaths_sim", 1:n_sim))
    colnames(predictions_modelaveraged) <- prediction_names
    predictions_modelaveraged[, 1] <- rowMeans(predictions_modelaveraged, na.rm = TRUE)

    bind_rows(
      .x,
      tibble(
        model_id = "MAV", model_class = "mav",
        model_para = list(NA),
        predictions = list(
          cbind(
            .x[1, ][["predictions"]][[1]] %>% select(-all_of(prediction_names)),
            predictions_modelaveraged
          ) %>% as_tibble()
        ),
        error_while_fit = FALSE
      )
    )
  }) %>%
  ungroup()

# Plot observed vs. predicted -------------------------------------

dat$fitted_models_with_mav %>%
  filter(!error_while_fit) %>%
  group_by(region_iso, model_id) %>%
  group_walk(~{

    cat(.y$region_iso, .y$model_id, '\n')

    predictions <- unnest(.x, predictions)
    expected_deaths <-
      predictions %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        deaths_observed = sum(deaths_observed),
        deaths_predicted = sum(deaths_predicted)
      ) %>%
      ungroup()
    simulated_deaths <-
      predictions %>%
      pivot_longer(cols = starts_with('deaths_sim'),
                   names_to = 'sim_id', values_to = 'deaths_sim') %>%
      group_by(cv_id, date, cv_sample, sim_id) %>%
      summarise(
        deaths_sim = sum(deaths_sim)
      ) %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        q05 = quantile(deaths_sim, 0.05, na.rm = TRUE),
        q95 = quantile(deaths_sim, 0.95, na.rm = TRUE)
      ) %>%
      ungroup()

    fig_dat <- left_join(expected_deaths, simulated_deaths)

    fig[[paste(.y[[1]], .y[[2]])]] <<-
      fig_dat %>%
      ggplot(aes(x = date)) +
      geom_ribbon(aes(ymin = q05, ymax = q95),
                  fill = 'grey70', color = NA) +
      geom_point(aes(color = cv_sample, y = deaths_observed),
                 size = 0.3) +
      geom_line(aes(y = deaths_predicted, alpha = cv_sample),
                color = 'red') +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      scale_alpha_manual(values = c(training = 0.3, test = 1)) +
      scale_color_manual(values = figspec$colors$sample) +
      facet_grid(cv_id~'') +
      guides(color = 'none', alpha = 'none') +
      figspec$MyGGplotTheme(grid = 'xy') +
      labs(
        x = NULL, y = 'Weekly deaths',
        title = paste(.y[[1]], .y[[2]])
      )
  })

# Exports ---------------------------------------------------------

saveRDS(dat$fitted_models_with_mav, file = path$expected_deaths_cv)

ggsave(
  filename = 'fitted_vs_observed_cv.pdf',
  path = 'tmp',
  plot = gridExtra::marrangeGrob(fig, nrow=1, ncol=1),
  width = 15, height = 9
)
