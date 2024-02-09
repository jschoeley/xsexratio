# Visualize excess deaths

# Init ------------------------------------------------------------

library(here)
library(tidyverse)
library(yaml)
library(glue)

wd <- here(); setwd(wd)

# paths
path <- list(
  # global objects
  glob = 'src/00-global_objects.R',
  # excess deaths
  excess_deaths = 'tmp/excess_deaths.rds',
  out = 'out',
  config = 'src/config.yaml'
)

cnst <- list(
  config = read_yaml(path$config),
  period = c(
    `20` = '2020w05-w26',
    `20/21` = '2020w27-21w26',
    `21/22` = '2021w27-22w26',
    `22/23` = '2022w27-23w26',
    `23` = '2023w27-w52'
  ),
  rate_scaler = 1e5
)

# global functions and constants
source(path$glob)

excess <- list()
fig <- list()

# Load data -------------------------------------------------------

excess$excess_measures <- readRDS(path$excess_deaths)

# P-score sexdiff epi-year ----------------------------------------

fig$pscoresexdiffepiyear <- list()

fig$pscoresexdiffepiyear$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US', 'PT'),
  measure_diff = 'psc_int_sdf',
  measure_f = 'psc_int_f',
  measure_m = 'psc_int_m'
)

fig$pscoresexdiffepiyear$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
   region_iso %in% fig$pscoresexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q05'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q75'),
    q95 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q95')
  )
fig$pscoresexdiffepiyear$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$pscoresexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q05'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q75'),
    q95 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q95')
  )
fig$pscoresexdiffepiyear$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$pscoresexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q05'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q75'),
    q95 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q95')
  )

fig$pscoresexdiffepiyear$fig <-
  fig$pscoresexdiffepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(ymax = q95, ymin = q05, y = q50), fatten = 1
  ) +
  geom_line(aes(y = q50, group = '1')) +
  geom_pointrange(
    aes(ymax = q95*1/4, ymin = q05*1/4, y = q50*1/4), fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffepiyear$data_f
  ) +
  geom_pointrange(
    aes(ymax = q95*1/4, ymin = q05*1/4, y = q50*1/4), fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$pscoresexdiffepiyear$data_m
  ) +
  scale_y_continuous(
    breaks = seq(-100, 100, 2.5),
    sec.axis = sec_axis(~.x*4, name = 'Male and female P-score')
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '') +
  #facet_grid(region_iso~age_group, scales = 'free_y') +
  facet_wrap(~region_iso) +
  labs(
#    title = 'Percentage point sex gap in male vs. female annual P-scores',
    y = 'P-score difference male - female',
    caption = paste(
      unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
      collapse = ', '
    ),
    x = NULL,
    family = 'roboto'
  )
fig$pscoresexdiffepiyear$fig

ExportFigure(
  fig$pscoresexdiffepiyear$fig, path = path$out,
  filename = '30-pscoresexdiffepiyear',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.8,
  scale = 1
)

# P-score sexdiff by age over epi-year ----------------------------

fig$pscoresexdiffageepiyear <- list()

fig$pscoresexdiffageepiyear$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US', 'PT'),
  measure_diff = 'psc_int_sdf',
  measure_f = 'psc_int_f',
  measure_m = 'psc_int_m'
)

fig$pscoresexdiffageepiyear$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$pscoresexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q05'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q75'),
    q95 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q95')
  )
fig$pscoresexdiffageepiyear$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$pscoresexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q05'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q75'),
    q95 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q95')
  )
fig$pscoresexdiffageepiyear$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$pscoresexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q05'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q75'),
    q95 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q95')
  )

fig$pscoresexdiffageepiyear$fig <-
  fig$pscoresexdiffageepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q95,
      ymin = q05,
      y = q50
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q95*1/4,
      ymin = q05*1/4,
      y = q50*1/4
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffageepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q95*1/4,
      ymin = q05*1/4,
      y = q50*1/4
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$pscoresexdiffageepiyear$data_m
  ) +
  scale_y_continuous(
    breaks = seq(-100, 100, 2.5),
    sec.axis = sec_axis(~.x*4, name = 'Male and female P-score',
                        breaks = seq(-100, 100, 2.5)*4)
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '', size = 5) +
  facet_grid(age_group~region_iso, scales = 'fixed') +
  labs(
    #title = 'Sex gap in male vs. female P-scores',
    y = 'P-score difference male - female',
    caption = paste(
      unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
      collapse = ', '
    ),
    x = NULL,
    family = 'roboto'
  )
fig$pscoresexdiffageepiyear$fig

ExportFigure(
  fig$pscoresexdiffageepiyear$fig, path = path$out,
  filename = '30-pscoresexdiffageepiyear',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.8,
  scale = 1
)

# Excess mortality sexdiff epi-year -------------------------------

fig$excessratesexdiffepiyear <- list()

fig$excessratesexdiffepiyear$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US', 'PT'),
  measure_diff = 'xr1_int_sdf',
  measure_f = 'xr1_int_f',
  measure_m = 'xr1_int_m'
)

fig$excessratesexdiffepiyear$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q05'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q75'),
    q95 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q95')
  )
fig$excessratesexdiffepiyear$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q05'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q75'),
    q95 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q95')
  )
fig$excessratesexdiffepiyear$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffepiyear$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q05 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q05'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q75'),
    q95 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q95')
  )

fig$excessratesexdiffepiyear$fig <-
  fig$excessratesexdiffepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q95*cnst$rate_scaler,
      ymin = q05*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q95*1/4*cnst$rate_scaler,
      ymin = q05*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q95*1/4*cnst$rate_scaler,
      ymin = q05*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$excessratesexdiffepiyear$data_m
  ) +
  scale_y_continuous(
    breaks = seq(-100, 300, 20),
    sec.axis =
      sec_axis(~.x*4, name = 'Female and male excess death rate (per 100,000 person-years)',
               breaks = seq(-100, 300, 20)*4)
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '') +
  #facet_grid(region_iso~age_group, scales = 'free_y') +
  facet_wrap(~region_iso) +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Excess death rate difference male - female (per 100,000 person-years)',
    caption = paste(
      unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
      collapse = ', '
    ),
    x = NULL,
    family = 'roboto'
  )
fig$excessratesexdiffepiyear$fig

ExportFigure(
  fig$excessratesexdiffepiyear$fig, path = path$out,
  filename = '30-excessratesexdiffepiyear',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.8,
  scale = 1
)

# Excess mortality sexdiff by age over epi-year -------------------

fig$excessratesexdiffageepiyear <- list()

fig$excessratesexdiffageepiyear$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US', 'PT'),
  measure_diff = 'xr1_int_sdf',
  measure_f = 'xr1_int_f',
  measure_m = 'xr1_int_m'
)

fig$excessratesexdiffageepiyear$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q05'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q75'),
    q95 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q95')
  )
fig$excessratesexdiffageepiyear$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q05'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q75'),
    q95 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q95')
  )
fig$excessratesexdiffageepiyear$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'epiyear') |>
  filter(
    region_iso %in% fig$excessratesexdiffageepiyear$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q05 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q05'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q75'),
    q95 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q95')
  )

fig$excessratesexdiffageepiyear$fig <-
  fig$excessratesexdiffageepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q95*cnst$rate_scaler,
      ymin = q05*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q95*1/4*cnst$rate_scaler,
      ymin = q05*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffageepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q95*1/4*cnst$rate_scaler,
      ymin = q05*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$excessratesexdiffageepiyear$data_m
  ) +
  scale_y_continuous(
    #breaks = seq(-200, 900, 50),
    sec.axis =
      sec_axis(~.x*4, name = 'Female and male excess death rate (per 100,000 person-years)',
               #breaks = seq(-200, 900, 50)*4
               )
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '', size = 5) +
  facet_grid(age_group~region_iso, scales = 'free_y') +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Excess death rate difference male - female (per 100,000 person-years)',
    caption = paste(
      unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
      collapse = ', '
    ),
    x = NULL,
    family = 'roboto'
  )
fig$excessratesexdiffageepiyear$fig

ExportFigure(
  fig$excessratesexdiffageepiyear$fig, path = path$out,
  filename = '30-excessratesexdiffageepiyear',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.8,
  scale = 1
)
