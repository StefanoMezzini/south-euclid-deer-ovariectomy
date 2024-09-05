library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ggplot2')   # for fancy plots
library('cowplot')   # for fancy multi-panel plots
library('mgcv')      # for GAMs
library('pammtools') # for piecewise-additive models
library('gratia')    # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

# import telemetry data (including T_169)
d <- readRDS('data/cleaned-telemetry-data.rds') %>%
  select(! tag_local_identifier) %>%
  unnest(tel) %>%
  filter(outlier == 0) %>%
  group_by(animal) %>% # to set time intervals correctly
  mutate(detected = 1,
         interval_h = c(NA, as.numeric(diff(timestamp), units = 'hours')),
         doy = lubridate::yday(timestamp),
         animal_year = paste(animal, lubridate::year(timestamp))) %>%
  ungroup() %>%
  filter(! is.na(interval_h)) %>%
  select(group, animal_year, interval_h, detected, doy)

ped <- as_ped(d, Surv(interval_h, detected) ~ group + animal_year + doy) %>%
  mutate(group = factor(group),
         animal_year = factor(animal_year))

if(file.exists('models/sampling-interval-hpam.rds')) {
  m <- readRDS('models/sampling-interval-hpam.rds')
} else {
  m <- pamm(
    ped_status ~
      group +
      s(animal_year, bs = 're') +
      s(doy, by = group, bs = 'cc', k = 7) +
      s(doy, animal_year, bs = 'fs', xt = list(bs = 'cr'), k = 7) +
      s(long, lat, bs = 'ds', k = 20),
    data = ped,
    engine = 'bam',
    method = 'fREML',
    discrete = TRUE,
    knots = list(doy = c(0.5, 366.5)),
    control = gam.control(trace = TRUE))
  saveRDS(m, 'models/sampling-interval-hpam.rds')
}

draw(m, rug = FALSE, parametric = TRUE)
qq.gam(m, type = 'pearson')
