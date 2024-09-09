library('readr')     # for importing csv data
library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('ggplot2')   # for fancy plots
library('cowplot')   # for fancy multi-panel plots
library('mgcv')      # for GAMs
library('gratia')    # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

# import data
d <- readRDS('data/cleaned-telemetry-data.rds') %>%
  select(! tag_local_identifier) %>%
  # add 
  left_join(
    read_csv('data/Odocoileus virginianus DeNicola South Euclid-reference-data.csv',
             show_col_types = FALSE, col_types = c(`animal-sex` = 'c')) %>%
      rename_with(\(.names) gsub('-', '_', .names)) %>%
      select(animal_id, deploy_on_date),
    by = join_by(animal == animal_id)) %>%
  relocate(deploy_on_date, .after = group) %>%
  unnest(tel) %>%
  select(group, animal, deploy_on_date, timestamp) %>%
  mutate(date = as.Date(timestamp)) %>%
  # calculate daily fixes per animal
  group_by(group, animal, deploy_on_date, date) %>%
  summarize(daily_fixes = n(), .groups = 'drop') %>%
  nest(tel = ! c(group, animal, deploy_on_date)) %>%
  # add days with no fixes and convert daily_fixes to 0
  mutate(tel = map2(tel, deploy_on_date, \(.t, .start) {
    full_t <- tibble(date = seq(.start, max(.t$date), by = 1))
    
    left_join(full_t, .t, by = 'date') %>%
      mutate(daily_fixes = if_else(is.na(daily_fixes), 0, daily_fixes))
  })) %>%
  unnest(tel) %>%
  mutate(group = factor(group),
         doy = lubridate::yday(date),
         doy_cr = doy,
         animal_year = factor(paste(animal, lubridate::year(date))))

# number of fixes per day are somewhat consistent between groups, other
# than one individual in the control group that has no fixes for a long
# period of time
ggplot(d, aes(daily_fixes, group = animal)) +
  facet_grid(group ~ .) +
  geom_density(color = '#00000080') +
  labs(x = 'Daily fixes', y = 'Density')

# this is clearly visible from the ECDF
ggplot(d, aes(daily_fixes, group = animal)) +
  facet_grid(group ~ .) +
  stat_ecdf(geom = 'step', alpha = 0.3) +
  labs(x = 'Daily fixes', y = 'Empirical cumulative density function')

if(file.exists('models/daily-fixes-hgam.rds')) {
  m <- readRDS('models/daily-fixes-hgam.rds')
} else {
  m <- bam(
    daily_fixes ~
      group +
      s(doy, by = group, bs = 'cc', k = 5) +
      s(doy_cr, animal_year, bs = 'fs', xt = list(bs = 'cr'), k = 5),
    family = tw(link = 'log'),
    data = d,
    method = 'fREML',
    discrete = TRUE,
    knots = list(doy = c(0.5, 365.5)))
  saveRDS(m, 'models/daily-fixes-hgam.rds')
}

appraise(m, point_alpha = 0.1)
draw(m, rug = FALSE, parametric = TRUE)
summary(m)

d %>%
  mutate(fitted = fitted(m)) %>%
  ggplot(aes(fitted, daily_fixes)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = group), alpha = 0.3) +
  geom_smooth(color = 'black') +
  scale_color_manual('Group', values = PAL) +
  labs(x = 'Fitted', y = 'Observed') +
  theme(legend.position = 'top')
