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
  config = read_yaml(path$config)
)

# global functions and constants
source(path$glob)

excess <- list()
fig <- list()

# Load data -------------------------------------------------------

excess$excess_measures <- readRDS(path$excess_deaths)

# P-score sexdiff -------------------------------------------------

fig$pscoresexdiffage <- list()

fig$pscoresexdiffage$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure = 'psc_wkl_sdf'
)

fig$pscoresexdiffage$data <-
  excess$excess_measures %>%
  filter(age_group != '[0,15)') %>%
  filter(timeframe == 'monthly') %>%
  filter(
   region_iso %in% fig$pscoresexdiffage$config$region_iso
  ) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0(fig$pscoresexdiffage$config$measure, '_q05'),
    q25 = paste0(fig$pscoresexdiffage$config$measure, '_q25'),
    q50 = paste0(fig$pscoresexdiffage$config$measure, '_q50'),
    q75 = paste0(fig$pscoresexdiffage$config$measure, '_q75'),
    q95 = paste0(fig$pscoresexdiffage$config$measure, '_q95')
  )

fig$pscoresexdiffage$fig <-
  fig$pscoresexdiffage$data %>%
  ggplot(aes(x = date, group = model_id)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = 'blue', alpha = 0.3
  ) +
  geom_line(aes(y = q50)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  scale_y_continuous(
    breaks = seq(-100, 100, 10)
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE, axis = '') +
  facet_grid(region_iso~age_group, scales = 'free_y') +
  #coord_cartesian(expand = FALSE, ylim = c(-10, 40)) +
  labs(
    x = NULL,
    title = 'Percentage point sex gap in male vs. female P-scores',
    y = 'P-score male - P-score female',
    family = 'roboto'
  )
fig$pscoresexdiffage$fig

ExportFigure(
  fig$pscoresexdiffage$fig, path = path$out, filename = 'pscoresexdiffage',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width, scale = 1.2
)

# P-score cumulative sexdiff --------------------------------------

fig$pscorecumsexdiffage <- list()

fig$pscorecumsexdiffage$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure = 'psc_cum_sdf'
)

fig$pscorecumsexdiffage$data <-
  excess$excess_measures %>%
  filter(age_group != '[0,15)') %>%
  filter(timeframe == 'monthly') %>%
  filter(
    region_iso %in% fig$pscorecumsexdiffage$config$region_iso
  ) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0(fig$pscorecumsexdiffage$config$measure, '_q05'),
    q25 = paste0(fig$pscorecumsexdiffage$config$measure, '_q25'),
    q50 = paste0(fig$pscorecumsexdiffage$config$measure, '_q50'),
    q75 = paste0(fig$pscorecumsexdiffage$config$measure, '_q75'),
    q95 = paste0(fig$pscorecumsexdiffage$config$measure, '_q95')
  )
fig$pscorecumsexdiffage$labels <-
  fig$pscorecumsexdiffage$data %>%
  drop_na(obs_wkl_t) %>%
  filter(date == max(date))

fig$pscorecumsexdiffage$fig <-
  fig$pscorecumsexdiffage$data %>%
  ggplot(aes(x = date, group = model_id)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = 'blue', alpha = 0.3
  ) +
  geom_line(aes(y = q50)) +
  # geom_line(
  #   aes(x = date, y = q50, group = group),
  #   data = fig$pscores$data %>% rename(group = model_id),
  #   inherit.aes = FALSE, alpha = 0.3
  # ) +
  geom_label(
    aes(
      x = date, y = -10,
      label = paste0(
        formatC(q50, format = 'f', flag = '+', digits = 2), '%', '\n',
        formatC(q05, format = 'f', digits = 2), '–',
        formatC(q95, format = 'f', digits = 2)
      )
    ),
    hjust = 1, size = 3, vjust = 1,
    label.padding = unit(1, 'pt'), label.size = 0, label.r = unit(0, 'pt'),
    data = fig$pscorecumsexdiffage$labels,
    inherit.aes = FALSE,
    color = 'black', alpha = 0.7
  ) +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  scale_y_continuous(
    breaks = seq(-100, 100, 5)
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE, axis = '') +
  facet_grid(region_iso~age_group) +
  #coord_cartesian(expand = FALSE, ylim = c(-10, 40)) +
  labs(
    title = 'Cumulative percentage point sex gap in male vs. female P-scores',
    y = 'Cumulative P-score male - Cumulative P-score female',
    family = 'roboto'
  )
fig$pscorecumsexdiffage$fig

ExportFigure(
  fig$pscorecumsexdiffage$fig, path = path$out, filename = 'pscorecumsexdiffage',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width, scale = 1.2
)

# P-score sex diff annual -----------------------------------------

fig$pscoresexdiffageannual <- list()

fig$pscoresexdiffageannual$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure = 'psc_wkl_sdf'
)

fig$pscoresexdiffageannual$data <-
  excess$excess_measures %>%
  filter(age_group != '[0,15)') %>%
  filter(timeframe == 'annual') %>%
  filter(
    region_iso %in% fig$pscoresexdiffageannual$config$region_iso
  ) %>%
  rename(
    q05 = paste0(fig$pscoresexdiffageannual$config$measure, '_q05'),
    q25 = paste0(fig$pscoresexdiffageannual$config$measure, '_q25'),
    q50 = paste0(fig$pscoresexdiffageannual$config$measure, '_q50'),
    q75 = paste0(fig$pscoresexdiffageannual$config$measure, '_q75'),
    q95 = paste0(fig$pscoresexdiffageannual$config$measure, '_q95')
  )
fig$pscoresexdiffageannual$labels <-
  fig$pscoresexdiffageannual$data %>%
  drop_na(obs_wkl_t) %>%
  filter(iso_year == max(iso_year))

fig$pscoresexdiffageannual$fig <-
  fig$pscoresexdiffageannual$data %>%
  ggplot(aes(x = iso_year, group = model_id)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_pointrange(
    aes(ymax = q95, ymin = q05, y = q50), fatten = 1
  ) +
  geom_line(aes(y = q50)) +
  # geom_line(
  #   aes(x = date, y = q50, group = group),
  #   data = fig$pscores$data %>% rename(group = model_id),
  #   inherit.aes = FALSE, alpha = 0.3
  # ) +
  #scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  scale_y_continuous(
    breaks = seq(-100, 100, 5)
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE, axis = '') +
  facet_grid(region_iso~age_group, scales = 'free_y') +
  #coord_cartesian(expand = FALSE, ylim = c(-10, 40)) +
  labs(
    title = 'Percentage point sex gap in male vs. female annual P-scores',
    y = 'P-score male - P-score female',
    x = NULL,
    family = 'roboto'
  )
fig$pscoresexdiffageannual$fig

ExportFigure(
  fig$pscoresexdiffageannual$fig, path = path$out, filename = 'pscoresexdiffageannual',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width, scale = 1.2
)

# Excess sexdiff --------------------------------------------------

fig$xc1sexdiff <- list()

fig$xc1sexdiff$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure = 'xc1_wkl_sdf'
)

fig$xc1sexdiff$data <-
  excess$excess_measures %>%
  filter(age_group != '[0,15)') %>%
  filter(timeframe == 'monthly') %>%
  filter(
    region_iso %in% fig$xc1sexdiff$config$region_iso
  ) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0(fig$xc1sexdiff$config$measure, '_q05'),
    q25 = paste0(fig$xc1sexdiff$config$measure, '_q25'),
    q50 = paste0(fig$xc1sexdiff$config$measure, '_q50'),
    q75 = paste0(fig$xc1sexdiff$config$measure, '_q75'),
    q95 = paste0(fig$xc1sexdiff$config$measure, '_q95')
  ) %>%
  mutate(across(c(q05, q25, q50, q75, q95), ~.x/personweeks)*1e5)
fig$xc1sexdiff$labels <-
  fig$xc1sexdiff$data %>%
  drop_na(obs_wkl_t) %>%
  filter(date == max(date))

fig$xc1sexdiff$fig <-
  fig$xc1sexdiff$data %>%
  ggplot(aes(x = date, group = model_id)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = 'blue', alpha = 0.3
  ) +
  geom_line(aes(y = q50)) +
  # geom_line(
  #   aes(x = date, y = q50, group = group),
  #   data = fig$pscores$data %>% rename(group = model_id),
  #   inherit.aes = FALSE, alpha = 0.3
  # ) +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  # scale_y_continuous(
  #   breaks = seq(-100, 100, 10)
  # ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE, axis = '') +
  facet_grid(region_iso~age_group, scales = 'free_y') +
  #coord_cartesian(expand = FALSE, ylim = c(-10, 40)) +
  labs(
    x = NULL, y = 'Percent excess deaths',
    title = 'Percent excess deaths Germany by age',
    family = 'roboto'
  )
fig$xc1sexdiff$fig

ExportFigure(
  fig$pscoresage$fig, path = path$out, filename = 'pscores_age_de',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width, scale = 1.2
)

# Excess by sex ---------------------------------------------------

fig$xc1bysex <- list()

fig$xc1bysex$config <- list(
  region_iso = c('BG', 'DE', 'HU', 'IT', 'NO', 'US'),
  measure = 'xc1_wkl_sdf'
)

fig$xc1bysex$data <-
  excess$excess_measures %>%
  filter(age_group != '[0,15)') %>%
  filter(timeframe == 'monthly') %>%
  filter(
    region_iso %in% fig$xc1bysex$config$region_iso
  ) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0(fig$xc1bysex$config$measure, '_q05'),
    q25 = paste0(fig$xc1bysex$config$measure, '_q25'),
    q50 = paste0(fig$xc1bysex$config$measure, '_q50'),
    q75 = paste0(fig$xc1bysex$config$measure, '_q75'),
    q95 = paste0(fig$xc1bysex$config$measure, '_q95')
  ) %>%
  mutate(across(c(q05, q25, q50, q75, q95), ~.x/personweeks)*1e5)
fig$xc1bysex$labels <-
  fig$xc1bysex$data %>%
  drop_na(obs_wkl_t) %>%
  filter(date == max(date))

fig$xc1bysex$fig <-
  fig$xc1bysex$data %>%
  ggplot(aes(x = date, group = model_id)) +
  geom_hline(yintercept = 0, color = 'grey70', size = 1) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = 'blue', alpha = 0.3
  ) +
  geom_line(aes(y = q50)) +
  # geom_line(
  #   aes(x = date, y = q50, group = group),
  #   data = fig$pscores$data %>% rename(group = model_id),
  #   inherit.aes = FALSE, alpha = 0.3
  # ) +
  scale_x_date(date_breaks = '1 year', date_labels = '%y') +
  # scale_y_continuous(
  #   breaks = seq(-100, 100, 10)
  # ) +
  scale_color_identity() +
  scale_fill_identity() +
  figspec$MyGGplotTheme(grid = 'xy', panel_border = TRUE, axis = '') +
  facet_grid(region_iso~age_group, scales = 'free_y') +
  #coord_cartesian(expand = FALSE, ylim = c(-10, 40)) +
  labs(
    x = NULL, y = 'Percent excess deaths',
    title = 'Percent excess deaths Germany by age',
    family = 'roboto'
  )
fig$xc1bysex$fig

ExportFigure(
  fig$pscoresage$fig, path = path$out, filename = 'pscores_age_de',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width, scale = 1.2
)
