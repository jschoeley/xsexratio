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
    `Pre-\nvacc.` = '2020/02-2021/04',
    `Post-\nvacc.` = '2021/05-2022/04',
    `Endemic` = '2022/05-2023/07'
  ),
  rate_scaler = 1e5
)

# global functions and constants
source(path$glob)

excess <- list()
fig <- list()

# Load data -------------------------------------------------------

excess$excess_measures <- readRDS(path$excess_deaths)

# P-score sexdiff over pandemic period ----------------------------

fig$pscoresexdiffpandemicperiod <- list()

fig$pscoresexdiffpandemicperiod$config <- list(
  #region_iso = c('BG', 'GB-EAW', 'DE', 'IT', 'NO', 'US'),
  region_iso = cnst$config$aggregates$analysis,
  measure_diff = 'psc_int_sdf',
  measure_f = 'psc_int_f',
  measure_m = 'psc_int_m',
  sec_axis_scaler = 4
)

fig$pscoresexdiffpandemicperiod$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_diff, '_q025'),
    q50 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_diff, '_q50'),
    q975 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_diff, '_q975')
  )
fig$pscoresexdiffpandemicperiod$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_f, '_q025'),
    q50 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_f, '_q50'),
    q975 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_f, '_q975')
  )
fig$pscoresexdiffpandemicperiod$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_m, '_q025'),
    q50 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_m, '_q50'),
    q975 = paste0(fig$pscoresexdiffpandemicperiod$config$measure_m, '_q975')
  )

fig$pscoresexdiffpandemicperiod$fig <-
  fig$pscoresexdiffpandemicperiod$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50, group = '1'), color = 'grey50') +
  geom_pointrange(
    aes(
      ymax = q975/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler,
      ymin = q025/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler,
      y = q50/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffpandemicperiod$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q975/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler,
      ymin = q025/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler,
      y = q50/fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$pscoresexdiffpandemicperiod$data_m
  ) +
  geom_pointrange(
    aes(ymax = q975, ymin = q025, y = q50), fatten = 2
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~.x*fig$pscoresexdiffpandemicperiod$config$sec_axis_scaler,
      name = 'Male and female P-score',
      breaks = function (limits) {
        labeling::extended(limits[1]/4, limits[2]/4, m = 5)*4
      }
    )
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = FALSE, axis = '') +
  theme(panel.background = element_rect(fill = 'grey95', colour = NA)) +
  facet_wrap(~region_iso, scales = 'free_y', ncol = 4) +
  labs(
#    title = 'Percentage point sex gap in male vs. female annual P-scores',
    y = 'P-score difference male - female',
    # caption = paste(
    #   unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
    #   collapse = ', '
    # ),
    x = NULL,
    family = 'roboto'
  )
fig$pscoresexdiffpandemicperiod$fig

ExportFigure(
  fig$pscoresexdiffpandemicperiod$fig, path = path$out,
  filename = '30-pscoresexdiffpandemicperiod',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*1.2,
  scale = 1
)

# P-score sexdiff by age over pandemic period ---------------------

fig$pscoresexdiffagepandemicperiod <- list()

fig$pscoresexdiffagepandemicperiod$config <- list(
  region_iso = c('BG', 'GB-EAW', 'DE', 'IT', 'NO', 'US'),
  measure_diff = 'psc_int_sdf',
  measure_f = 'psc_int_f',
  measure_m = 'psc_int_m',
  sec_axis_scaler = 4
)

fig$pscoresexdiffagepandemicperiod$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_diff, '_q025'),
    q50 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_diff, '_q50'),
    q975 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_diff, '_q975')
  )
fig$pscoresexdiffagepandemicperiod$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_f, '_q025'),
    q50 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_f, '_q50'),
    q975 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_f, '_q975')
  )
fig$pscoresexdiffagepandemicperiod$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$pscoresexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_m, '_q025'),
    q50 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_m, '_q50'),
    q975 = paste0(fig$pscoresexdiffagepandemicperiod$config$measure_m, '_q975')
  )

fig$pscoresexdiffagepandemicperiod$fig <-
  fig$pscoresexdiffagepandemicperiod$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50, group = '1'), color = 'grey50') +
  geom_pointrange(
    aes(
      ymax = q975/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
      ymin = q025/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
      y = q50/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffagepandemicperiod$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q975/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
      ymin = q025/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
      y = q50/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$pscoresexdiffagepandemicperiod$data_m
  ) +
  geom_pointrange(
    aes(ymax = q975, ymin = q025, y = q50), fatten = 2
  ) +
  scale_y_continuous(
    #breaks = seq(-100, 100, 2.5),
    sec.axis = sec_axis(
      ~.x*fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
      name = 'Male and female P-score',
      breaks = function (limits) {
        labeling::extended(
          limits[1]/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
          limits[2]/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
          m = 5)*fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler
      }
    )
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = FALSE, axis = '', size = 5) +
  theme(panel.background = element_rect(fill = 'grey95', colour = NA)) +
  facet_wrap(~region_iso+age_group, scales = 'free_y', ncol = 4) +
  labs(
    #title = 'Sex gap in male vs. female P-scores',
    y = 'P-score difference male - female',
    # caption = paste(
    #   unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
    #   collapse = ', '
    # ),
    x = NULL,
    family = 'roboto'
  )
fig$pscoresexdiffagepandemicperiod$fig

ExportFigure(
  fig$pscoresexdiffagepandemicperiod$fig, path = path$out,
  filename = '30-pscoresexdiffagepandemicperiod',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width,
  scale = 1
)

# Excess mortality sexdiff over pandemic period -------------------

fig$excessratesexdiffpandemicperiod <- list()

fig$excessratesexdiffpandemicperiod$config <- list(
  #region_iso = c('BG', 'GB-EAW', 'DE', 'IT', 'NO', 'US'),
  region_iso = cnst$config$aggregates$analysis,
  measure_diff = 'xr1_int_sdf',
  measure_f = 'xr1_int_f',
  measure_m = 'xr1_int_m',
  sec_axis_scaler = 4
)

fig$excessratesexdiffpandemicperiod$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_diff, '_q025'),
    q50 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_diff, '_q50'),
    q975 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_diff, '_q975')
  )
fig$excessratesexdiffpandemicperiod$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_f, '_q025'),
    q50 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_f, '_q50'),
    q975 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_f, '_q975')
  )
fig$excessratesexdiffpandemicperiod$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffpandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_m, '_q025'),
    q50 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_m, '_q50'),
    q975 = paste0(fig$excessratesexdiffpandemicperiod$config$measure_m, '_q975')
  )

fig$excessratesexdiffpandemicperiod$fig <-
  fig$excessratesexdiffpandemicperiod$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = (q975*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler,
      ymin = (q025*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler,
      y = (q50*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffpandemicperiod$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = (q975*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler,
      ymin = (q025*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler,
      y = (q50*cnst$rate_scaler)/fig$excessratesexdiffpandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$excessratesexdiffpandemicperiod$data_m
  ) +
  geom_pointrange(
    aes(
      ymax = q975*cnst$rate_scaler,
      ymin = q025*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1')) +
  scale_y_continuous(
    sec.axis =
      sec_axis(
        ~.x*fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler, name = 'Female and male excess death rate (per 100,000 person-years)',
        breaks = function (limits) {
          labeling::extended(
            limits[1]/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
            limits[2]/fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler,
            m = 5)*fig$pscoresexdiffagepandemicperiod$config$sec_axis_scaler
        }
      )
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = FALSE, axis = '', size = 5) +
  theme(panel.background = element_rect(fill = 'grey95', colour = NA)) +
  #facet_grid(region_iso~age_group, scales = 'free_y') +
  facet_wrap(~region_iso, ncol = 4, scales = 'free_y') +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Excess death rate difference male - female (per 100,000 person-years)',
    # caption = paste(
    #   unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
    #   collapse = ', '
    # ),
    x = NULL,
    family = 'roboto'
  )
fig$excessratesexdiffpandemicperiod$fig

ExportFigure(
  fig$excessratesexdiffpandemicperiod$fig, path = path$out,
  filename = '30-excessratesexdiffpandemicperiod',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*1.2,
  scale = 1
)

# Excess mortality sexdiff by age over epi-year -------------------

fig$excessratesexdiffagepandemicperiod <- list()

fig$excessratesexdiffagepandemicperiod$config <- list(
  region_iso = c('BG', 'GB-EAW', 'DE', 'IT', 'NO', 'US'),
  measure_diff = 'xr1_int_sdf',
  measure_f = 'xr1_int_f',
  measure_m = 'xr1_int_m',
  sec_axis_scaler = 4
)

fig$excessratesexdiffagepandemicperiod$data_diff <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_diff, '_q025'),
    q50 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_diff, '_q50'),
    q975 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_diff, '_q975')
  )
fig$excessratesexdiffagepandemicperiod$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_f, '_q025'),
    q50 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_f, '_q50'),
    q975 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_f, '_q975')
  )
fig$excessratesexdiffagepandemicperiod$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value =
           factor(timeframe_value, cnst$period, names(cnst$period))
  ) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'pandemicperiod') |>
  filter(
    region_iso %in% fig$excessratesexdiffagepandemicperiod$config$region_iso
  ) |>
  select(
    region_iso, age_group, timeframe_value,
    q025 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_m, '_q025'),
    q50 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_m, '_q50'),
    q975 = paste0(fig$excessratesexdiffagepandemicperiod$config$measure_m, '_q975')
  )

fig$excessratesexdiffagepandemicperiod$fig <-
  fig$excessratesexdiffagepandemicperiod$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1'), color = 'grey50') +
  geom_pointrange(
    aes(
      ymax = q975*cnst$rate_scaler,
      ymin = q025*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 2
  ) +
  geom_pointrange(
    aes(
      ymax = (q975*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
      ymin = (q025*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
      y = (q50*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffagepandemicperiod$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = (q975*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
      ymin = (q025*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
      y = (q50*cnst$rate_scaler)/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Male'],
    position = position_nudge(x = -0.1),
    data = fig$excessratesexdiffagepandemicperiod$data_m
  ) +
  scale_y_continuous(
    sec.axis =
      # this is a second axis which alignes with the tick
      # marks of the first axis but is scaled to four times
      # the range of the first
      sec_axis(
        ~.x*fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
        name = 'Female and male excess death rate (per 100,000 person-years)',
        breaks = function (limits) {
          labeling::extended(
            limits[1]/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler,
            limits[2]/fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler, m = 5)*fig$excessratesexdiffagepandemicperiod$config$sec_axis_scaler
        }
      )
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = FALSE, axis = '', size = 5) +
  theme(panel.background = element_rect(fill = 'grey95', colour = NA)) +
  facet_wrap(~region_iso+age_group, scales = 'free_y', ncol = 4) +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Excess death rate difference male - female (per 100,000 person-years)',
    # caption = paste(
    #   unlist(map2(names(cnst$period), cnst$period, paste, sep = ': ')),
    #   collapse = ', '
    # ),
    x = NULL,
    family = 'roboto'
  )
fig$excessratesexdiffagepandemicperiod$fig

ExportFigure(
  fig$excessratesexdiffagepandemicperiod$fig, path = path$out,
  filename = '30-excessratesexdiffagepandemicperiod',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width,
  scale = 1
)
