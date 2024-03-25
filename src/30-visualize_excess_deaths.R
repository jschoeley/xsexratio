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
  rate_scaler = 1e5,
  start_of_vax_phase = '2021-03-22',
  start_of_endemic_phase = '2022-03-21'
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
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
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
    q025 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q025'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q75'),
    q975 = paste0(fig$pscoresexdiffepiyear$config$measure_diff, '_q975')
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
    q025 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q025'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q75'),
    q975 = paste0(fig$pscoresexdiffepiyear$config$measure_f, '_q975')
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
    q025 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q025'),
    q25 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q75'),
    q975 = paste0(fig$pscoresexdiffepiyear$config$measure_m, '_q975')
  )

fig$pscoresexdiffepiyear$fig <-
  fig$pscoresexdiffepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(ymax = q975, ymin = q025, y = q50), fatten = 1
  ) +
  geom_line(aes(y = q50, group = '1')) +
  geom_pointrange(
    aes(ymax = q975*1/4, ymin = q025*1/4, y = q50*1/4), fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffepiyear$data_f
  ) +
  geom_pointrange(
    aes(ymax = q975*1/4, ymin = q025*1/4, y = q50*1/4), fatten = 1,
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
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
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
    q025 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q025'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q75'),
    q975 = paste0(fig$pscoresexdiffageepiyear$config$measure_diff, '_q975')
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
    q025 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q025'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q75'),
    q975 = paste0(fig$pscoresexdiffageepiyear$config$measure_f, '_q975')
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
    q025 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q025'),
    q25 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q75'),
    q975 = paste0(fig$pscoresexdiffageepiyear$config$measure_m, '_q975')
  )

fig$pscoresexdiffageepiyear$fig <-
  fig$pscoresexdiffageepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q975,
      ymin = q025,
      y = q50
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q975*1/4,
      ymin = q025*1/4,
      y = q50*1/4
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$pscoresexdiffageepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q975*1/4,
      ymin = q025*1/4,
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
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
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
    q025 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q025'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q75'),
    q975 = paste0(fig$excessratesexdiffepiyear$config$measure_diff, '_q975')
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
    q025 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q025'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q75'),
    q975 = paste0(fig$excessratesexdiffepiyear$config$measure_f, '_q975')
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
    q025 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q025'),
    q25 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q75'),
    q975 = paste0(fig$excessratesexdiffepiyear$config$measure_m, '_q975')
  )

fig$excessratesexdiffepiyear$fig <-
  fig$excessratesexdiffepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q975*cnst$rate_scaler,
      ymin = q025*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q975*1/4*cnst$rate_scaler,
      ymin = q025*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q975*1/4*cnst$rate_scaler,
      ymin = q025*1/4*cnst$rate_scaler,
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
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
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
    q025 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q025'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q75'),
    q975 = paste0(fig$excessratesexdiffageepiyear$config$measure_diff, '_q975')
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
    q025 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q025'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q75'),
    q975 = paste0(fig$excessratesexdiffageepiyear$config$measure_f, '_q975')
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
    q025 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q025'),
    q25 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q25'),
    q50 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q50'),
    q75 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q75'),
    q975 = paste0(fig$excessratesexdiffageepiyear$config$measure_m, '_q975')
  )

fig$excessratesexdiffageepiyear$fig <-
  fig$excessratesexdiffageepiyear$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(
      ymax = q975*cnst$rate_scaler,
      ymin = q025*cnst$rate_scaler,
      y = q50*cnst$rate_scaler
    ),
    fatten = 1
  ) +
  geom_line(aes(y = q50*cnst$rate_scaler, group = '1')) +
  geom_pointrange(
    aes(
      ymax = q975*1/4*cnst$rate_scaler,
      ymin = q025*1/4*cnst$rate_scaler,
      y = q50*1/4*cnst$rate_scaler
    ),
    fatten = 1,
    color = figspec$colors$sex['Female'],
    position = position_nudge(x = 0.1),
    data = fig$excessratesexdiffageepiyear$data_f
  ) +
  geom_pointrange(
    aes(
      ymax = q975*1/4*cnst$rate_scaler,
      ymin = q025*1/4*cnst$rate_scaler,
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

# Draft figure 1a -------------------------------------------------

fig$draftfig1a <- list()

fig$draftfig1a$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure_f = 'xr1_int_f',
  measure_m = 'xr1_int_m'
)

fig$draftfig1a$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig1a$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$draftfig1a$config$measure_f, '_q025'),
    q25 = paste0(fig$draftfig1a$config$measure_f, '_q25'),
    q50 = paste0(fig$draftfig1a$config$measure_f, '_q50'),
    q75 = paste0(fig$draftfig1a$config$measure_f, '_q75'),
    q975 = paste0(fig$draftfig1a$config$measure_f, '_q975')
  )
fig$draftfig1a$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig1a$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$draftfig1a$config$measure_m, '_q025'),
    q25 = paste0(fig$draftfig1a$config$measure_m, '_q25'),
    q50 = paste0(fig$draftfig1a$config$measure_m, '_q50'),
    q75 = paste0(fig$draftfig1a$config$measure_m, '_q75'),
    q975 = paste0(fig$draftfig1a$config$measure_m, '_q975')
  )

fig$draftfig1a$fig <-
  fig$draftfig1a$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50*cnst$rate_scaler/12),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig1a$data_m) +
  geom_line(aes(y = q50*cnst$rate_scaler/12),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig1a$data_f) +
  scale_y_continuous(
    #breaks = seq(-200, 900, 50),
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '') +
  facet_wrap(~region_iso) +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Excess death rate (per 100,000 person-years)',
    x = NULL,
    family = 'roboto'
  )
fig$draftfig1a$fig

ExportFigure(
  fig$draftfig1a$fig, path = path$out,
  filename = '30-draftfig1a',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.5,
  scale = 1
)

# Draft figure 1b -------------------------------------------------

fig$draftfig1b <- list()

fig$draftfig1b$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure_f = 'psc_int_f',
  measure_m = 'psc_int_m'
)

fig$draftfig1b$data_f <-
  excess$excess_measures |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig1b$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$draftfig1b$config$measure_f, '_q025'),
    q25 = paste0(fig$draftfig1b$config$measure_f, '_q25'),
    q50 = paste0(fig$draftfig1b$config$measure_f, '_q50'),
    q75 = paste0(fig$draftfig1b$config$measure_f, '_q75'),
    q975 = paste0(fig$draftfig1b$config$measure_f, '_q975')
  )
fig$draftfig1b$data_m <-
  excess$excess_measures |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  #filter(age_group != '[0,15)') |>
  filter(age_group == 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig1b$config$region_iso
  ) |>
  select(
    region_iso, timeframe_value,
    q025 = paste0(fig$draftfig1b$config$measure_m, '_q025'),
    q25 = paste0(fig$draftfig1b$config$measure_m, '_q25'),
    q50 = paste0(fig$draftfig1b$config$measure_m, '_q50'),
    q75 = paste0(fig$draftfig1b$config$measure_m, '_q75'),
    q975 = paste0(fig$draftfig1b$config$measure_m, '_q975')
  )

fig$draftfig1b$fig <-
  fig$draftfig1b$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig1b$data_m) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig1b$data_f) +
  scale_y_continuous(
    #breaks = seq(-200, 900, 50),
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '') +
  facet_wrap(~region_iso) +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Percent excess deaths',
    x = NULL,
    family = 'roboto'
  )
fig$draftfig1b$fig

ExportFigure(
  fig$draftfig1b$fig, path = path$out,
  filename = '30-draftfig1b',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.5,
  scale = 1
)

# Draft figure 4 --------------------------------------------------

fig$draftfig4 <- list()

fig$draftfig4$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure_f = 'xr1_cum_f',
  measure_m = 'xr1_cum_m'
)

fig$draftfig4$data_pre <-
  excess$excess_measures |>
  filter(iso_year < 2023) |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig4$config$region_iso
  )

fig$draftfig4$data_f_prevax <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value <= cnst$start_of_vax_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  # only do with median, for prediction intervals re-implement measure
  mutate(q50 = q50-first(q50))
fig$draftfig4$data_f_postvax <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value > cnst$start_of_vax_phase,
         timeframe_value <= cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig4$data_f_endemic <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value > cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig4$data_m_prevax <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value <= cnst$start_of_vax_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  # only do with median, for prediction intervals re-implement measure
  mutate(q50 = q50-first(q50))
fig$draftfig4$data_m_postvax <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value > cnst$start_of_vax_phase,
         timeframe_value <= cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig4$data_m_endemic <-
  fig$draftfig4$data_pre |>
  filter(timeframe_value > cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig4$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))

fig$draftfig4$fig <-
  fig$draftfig4$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig4$data_m_prevax) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig4$data_m_postvax) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig4$data_m_endemic) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig4$data_f_prevax) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig4$data_f_postvax) +
  geom_line(aes(y = q50*cnst$rate_scaler),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig4$data_f_endemic) +
  scale_y_continuous(
    #breaks = seq(-200, 900, 50),
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '') +
  facet_grid(age_group~region_iso, scales = 'free_y') +
  labs(
    #title = 'Sex gap in male vs. female excess death rates',
    y = 'Cumulative excess death rates (per 100,000 person-years)',
    x = NULL,
    family = 'roboto'
  )
fig$draftfig4$fig

ExportFigure(
  fig$draftfig4$fig, path = path$out,
  filename = '30-draftfig4',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.5,
  scale = 1
)

# Draft figure 5 --------------------------------------------------

fig$draftfig5 <- list()

fig$draftfig5$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure_f = 'psc_cum_f',
  measure_m = 'psc_cum_m'
)

fig$draftfig5$data_pre <-
  excess$excess_measures |>
  filter(iso_year < 2023) |>
  mutate(timeframe_value = ISOWeekDateToDate(iso_year, iso_week)) |>
  filter(age_group != '[0,15)') |>
  filter(age_group != 'Total') |>
  filter(timeframe == 'monthly') |>
  filter(
    region_iso %in% fig$draftfig5$config$region_iso
  )

fig$draftfig5$data_f_prevax <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value <= cnst$start_of_vax_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  # only do with median, for prediction intervals re-implement measure
  mutate(q50 = q50-first(q50))
fig$draftfig5$data_f_postvax <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value > cnst$start_of_vax_phase,
         timeframe_value <= cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig5$data_f_endemic <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value > cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_f, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig5$data_m_prevax <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value <= cnst$start_of_vax_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  # only do with median, for prediction intervals re-implement measure
  mutate(q50 = q50-first(q50))
fig$draftfig5$data_m_postvax <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value > cnst$start_of_vax_phase,
         timeframe_value <= cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))
fig$draftfig5$data_m_endemic <-
  fig$draftfig5$data_pre |>
  filter(timeframe_value > cnst$start_of_endemic_phase) |>
  select(
    region_iso, age_group, timeframe_value,
    q50 = paste0(fig$draftfig5$config$measure_m, '_q50')
  ) |>
  group_by(region_iso, age_group) |>
  mutate(q50 = q50-first(q50))

fig$draftfig5$fig <-
  fig$draftfig5$data_diff |>
  ggplot(aes(x = timeframe_value)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 0.2) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig5$data_m_prevax) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig5$data_m_postvax) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Male'],
            data = fig$draftfig5$data_m_endemic) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig5$data_f_prevax) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig5$data_f_postvax) +
  geom_line(aes(y = q50),
            color = figspec$colors$sex['Female'],
            data = fig$draftfig5$data_f_endemic) +
  scale_y_continuous(
    #breaks = seq(-200, 900, 50),
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'y', panel_border = TRUE, axis = '', size = 5) +
  facet_grid(age_group~region_iso, scales = 'free_y') +
  labs(
    y = 'Percent excess deaths',
    x = NULL,
    family = 'roboto'
  )
fig$draftfig5$fig

ExportFigure(
  fig$draftfig5$fig, path = path$out,
  filename = '30-draftfig5',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.5,
  scale = 1
)
