library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('ctmm')      # for calculating excursivity
library('ggplot2')   # for fancy plots
library('cowplot')   # for fancy multi-panel plots
library('mgcv')      # for GAMs
library('gratia')    # for ggplot-based model plots
library('lubridate') # for working with dates
source('analysis/figures/default-ggplot-theme.R')
source('functions/get_cis.R')
source('functions/get_preds.R')
source('functions/get_legend.R')

# import moving window data ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  select(! c(hr_lwr_95, hr_upr_95)) %>%
  mutate(doy = yday(date),
         group = if_else(group == 'Ovariectomy', 'Treatment', group) %>%
           factor(),
         animal_year = paste(animal, year(date)) %>%
           factor())

# daily excursivity data ----
d <- readRDS('models/full-telemetry-movement-models.rds') %>%
  mutate(tel = map2(tel, ud, \(.t, .u) {
    .r <- raster(.u, DF = 'CDF')
    .t <- data.frame(.t) %>%
      mutate(excursivity = raster::extract(.r, SpatialPoints.telemetry(.t)))
    return(.t)
  })) %>%
  select(animal, group, tel) %>%
  unnest(tel) %>%
  mutate(animal = factor(animal),
         group = if_else(group == 'ovariectomy', 'treatment', group) %>%
           stringr::str_to_sentence() %>%
           factor(),
         doy = yday(timestamp),
         date = as.Date(timestamp)) %>%
  group_by(group, animal, doy, date) %>%
  summarize(excursivity = mean(excursivity)) %>%
  ungroup() %>%
  mutate(animal_year = factor(paste(animal, year(date))))

# import number of daily fixes
fixes <- readRDS('data/cleaned-telemetry-data.rds') %>%
  select(! tag_local_identifier) %>%
  unnest(tel) %>%
  filter(outlier == 0) %>%
  select(group, animal, timestamp) %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(group, animal, date) %>%
  summarize(daily_fixes = n(), .groups = 'drop') %>%
  nest(tel = ! c(group, animal)) %>%
  mutate(tel = map(tel, \(.t) {
    full_t <- tibble(date = seq(min(.t$date), max(.t$date), by = 1))
    
    left_join(full_t, .t, by = 'date') %>%
      mutate(daily_fixes = if_else(is.na(daily_fixes), 0, daily_fixes))
  })) %>%
  unnest(tel) %>%
  mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group) %>%
           factor(),
         doy = yday(date),
         animal_year = factor(paste(animal, year(date))))

#' figure of `n_fixes` with period in which the library station was down
#' **DOES NOT ACCOUNT FOR INDIVIDUALS**
ggplot(fixes, aes(date, daily_fixes)) +
  facet_grid(group ~ .) +
  geom_vline(xintercept = date('2023-05-15'), color = 'red4') +
  geom_vline(xintercept = date('2023-07-11'), color = 'red4') +
  geom_point(aes(color = factor(date > date('2023-05-15') &
                                  date < date('2023-07-11'))),
             alpha = 0.2) +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 30, bs = 'ad'),
              color = 'dodgerblue2', n = 400,
              method.args = list(family = tw(link = 'log'))) +
  labs(x = NULL, y = 'Number of fixes per deer in a day') +
  scale_color_manual('Library station down', values = 1:2,
                     labels = c('No', 'Yes')) +
  theme(legend.position = 'none')

# import models ----
m_hr <- readRDS('models/m-hr.rds')
m_diff <- readRDS('models/m-diff.rds')
m_exc <- readRDS('models/m-exc.rds')
m_fix <- readRDS('models/daily-fixes-hgam.rds')

# make predictions ----
newd <- expand_grid(group = unique(m_hr$model$group),
                    doy = seq(1, 365, length.out = 400),
                    doy_cr = 0,
                    animal_year = 'new animal')

preds <- bind_cols(newd,
                   get_preds(parameter = 'hr'),
                   get_preds(parameter = 'diff'),
                   get_preds(parameter = 'exc'),
                   get_preds(parameter = 'fix')) %>%
  mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group))

# get estimates and CIs for the intercept values ----
get_cis(m_fix)
get_cis(m_hr)
get_cis(m_diff)
get_cis(m_exc)

# make figures ----
doy_breaks <- c(1, 91, 182, 274)
doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%B 1')

theme_set(theme_get() +
            theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), units = 'pt')))

# with data points ----
# movement behavior parameters
filter(mw, hr_est_95 > 10 | diffusion_km2_day > 2) %>%
  select(group, animal, hr_est_95, diffusion_km2_day, date)

p_hr <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  # dropping one extreme HR estimates
  geom_point(aes(doy, hr_est_95), filter(mw, hr_est_95 < 10),
             alpha = 0.2, na.rm = TRUE) +
  geom_ribbon(aes(doy, ymin = hr_lwr_95, ymax = hr_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = hr_lwr_50, ymax = hr_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, hr_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('7-day home-range size (km\U00B2)') +
  ylim(c(0, 7.5)) +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill'),
                    labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

p_diff <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  # dropping one extreme estimate from the plot for readability
  geom_point(aes(doy, diffusion_km2_day),
             filter(mw, diffusion_km2_day < 2), alpha = 0.2, na.rm = TRUE) +
  geom_ribbon(aes(doy, ymin = diff_lwr_95, ymax = diff_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = diff_lwr_50, ymax = diff_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, diff_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Daily diffusion rate (km\U00B2/day)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_exc <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_point(aes(doy, excursivity), d, alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = exc_lwr_95, ymax = exc_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = exc_lwr_50, ymax = exc_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, exc_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  scale_y_continuous('Daily excursivity', limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

plot_grid(p_hr, p_diff, p_exc, labels = 'auto', ncol = 1)

ggsave('figures/hgam-figure-with-data.png', width = 16, height = 12,
       units = 'in', dpi = 600, bg = 'white')

# daily fixes
p_fix <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_point(aes(doy, daily_fixes), fixes, alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = fix_lwr_95, ymax = fix_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = fix_lwr_50, ymax = fix_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, fix_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Number of fixes per deer in a day') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

ggsave('figures/daily-fixes-figure-with-data.png', p_fix,
       width = 16, height = 6, units = 'in', dpi = 600, bg = 'white')

# without data points ----
p_hr <-
  ggplot(preds) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_ribbon(aes(doy, ymin = hr_lwr_95, ymax = hr_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = hr_lwr_50, ymax = hr_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, hr_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('7-day home-range size (km\U00B2)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_diff <-
  ggplot(preds) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_ribbon(aes(doy, ymin = diff_lwr_95, ymax = diff_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = diff_lwr_50, ymax = diff_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, diff_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Daily diffusion rate (km\U00B2/day)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_exc <-
  ggplot(preds) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_ribbon(aes(doy, ymin = exc_lwr_95, ymax = exc_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = exc_lwr_50, ymax = exc_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, exc_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  scale_y_continuous('Daily excursivity') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

leg <- get_legend(p_hr)

plot_grid(leg, p_hr, p_diff, p_exc, labels = c('', 'a', 'b', 'c'),
          ncol = 1, rel_heights = c(0.1, 1, 1, 1))

ggsave('figures/hgam-figure.png', width = 8, height = 12, units = 'in',
       dpi = 600, bg = 'white')

# daily fixes
p_fix <-
  ggplot(preds) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_ribbon(aes(doy, ymin = fix_lwr_95, ymax = fix_upr_95, fill = group),
              alpha = 0.2) +
  geom_ribbon(aes(doy, ymin = fix_lwr_50, ymax = fix_upr_50, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, fix_mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Number of fixes per deer in a day') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'top')

ggsave('figures/daily-fixes-figure.png', p_fix,
       width = 8, height = 6, units = 'in', dpi = 600, bg = 'white')
