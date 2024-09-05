library('ctmm')      #' for `%#%` operator (unit conversion)
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ggplot2')   # for fancy plots
library('cowplot')   # for fancy multi-panel plots
library('mgcv')      # for GAMs
library('gratia')    # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

# import telemetry data
d <- readRDS('data/cleaned-telemetry-data.rds') %>%
  select(! tag_local_identifier) %>%
  unnest(tel) %>%
  filter(outlier == 0) %>%
  group_by(animal) %>% # to set time intervals correctly
  mutate(interval_h = c(NA, as.numeric(diff(timestamp), units = 'hours')),
         doy = lubridate::yday(timestamp)) %>%
  ungroup() %>%
  filter(! is.na(interval_h)) %>%
  mutate(group = factor(group),
         animal = factor(animal),
         animal_year = factor(paste(animal, lubridate::year(timestamp))))

# treatment group has almost four times as many gaps > 12 hours
# but the difference in proportions is small
d %>%
  group_by(group) %>%
  summarize(prop = mean(interval_h > 12))

# this is clearly visible from the ECDF
ggplot(d, aes(interval_h, group = animal)) +
  facet_grid(group ~ .) +
  stat_ecdf(geom = 'step', pad = FALSE, alpha = 0.3) +
  scale_x_continuous('Interval between fixes (hours, log scale)',
                     transform = 'log', breaks = c(1, 10, 100, 1e3)) +
  ylab('Empirical cumulative density function')

# still, the extreme values make modeling the gaps complicated

if(file.exists('models/sampling-interval-hgam.rds')) {
  m <- readRDS('models/sampling-interval-hgam.rds')
} else {
  # fits in ~50 iterations
  m <- bam(
    interval_h ~
      group +
      s(animal, bs = 're') +
      s(doy, by = group, bs = 'cc', k = 7) +
      s(doy, animal_year, bs = 'fs', xt = list(bs = 'cr'), k = 7) +
      s(long, lat, bs = 'ds', k = 20),
    family = Gamma(link = 'log'),
    data = d,
    method = 'fREML',
    discrete = TRUE,
    control = gam.control(trace = TRUE))
  saveRDS(m, 'models/sampling-interval-hgam.rds')
}

draw(m, rug = FALSE, parametric = TRUE)
appraise(m, point_alpha = 0.1)

d <- mutate(d, e = resid(m))

d %>%
  filter(e > 2.5) %>%
  pull(interval_h) %>%
  hist()

# some very long tails that make the models unstable, as well as a possible
# mixture of 3 distributions (see the 3 segments: s1 < -2.5 < s2 < 1 < s3)
ggplot(d, aes(sample = e)) +
  facet_wrap(~ group) +
  geom_qq_line(color = 'red') +
  geom_qq(shape = '.')

summary(m)
