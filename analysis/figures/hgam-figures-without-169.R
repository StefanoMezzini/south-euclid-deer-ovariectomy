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

# import moving window data ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  filter(! grepl('T_169', animal)) %>%
  select(! c(hr_lwr_95, hr_upr_95)) %>%
  mutate(doy = yday(date),
         group = if_else(group == 'Ovariectomy', 'Treatment', group) %>%
           factor(),
         animal_year = paste(animal, year(date)) %>%
           factor())

# daily excursivity data ----
d <- readRDS('models/full-telemetry-movement-models.rds') %>%
  filter(! grepl('T_169', animal)) %>%
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

# import models ----
m_hr <- readRDS('models/m-hr-without-T_169.rds')
m_diff <- readRDS('models/m-diff-without-T_169.rds')
m_exc <- readRDS('models/m-exc-without-T_169.rds')

# make predictions ----
newd <- expand_grid(group = unique(m_hr$model$group),
                    doy = seq(1, 365, length.out = 400),
                    doy_cr = 0,
                    animal_year = 'new animal')

preds <- bind_cols(newd,
                   get_preds(parameter = 'hr'),
                   get_preds(parameter = 'diff'),
                   get_preds(parameter = 'exc')) %>%
  mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group))

# get estimates and CIs for the intercept values ----
# without T_169
get_cis(m_hr)
get_cis(m_diff)
get_cis(m_exc)

# with T_169
get_cis(readRDS('models/m-hr-with-T_169.rds'))
get_cis(readRDS('models/m-diff-with-T_169.rds'))
get_cis(readRDS('models/m-exc-with-T_169.rds'))

# make figures ----
doy_breaks <- c(1, 91, 182, 274)
doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%B 1')

theme_set(theme_get() +
            theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), units = 'pt')))

# with data points ----
# movement behavior parameters
filter(mw, hr_est_95 > 10) %>%
  select(group, animal, hr_est_95, date)

p_hr <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_vline(xintercept = yday('2023-05-30'), color = 'grey') +
  geom_vline(xintercept = yday('2023-11-10'), color = 'grey') +
  geom_point(aes(doy, hr_est_95), mw, alpha = 0.2, na.rm = TRUE) +
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
  geom_point(aes(doy, diffusion_km2_day), mw, alpha = 0.2, na.rm = TRUE) +
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

ggsave('figures/hgam-figure-with-data-without-T_169.png',
       width = 16, height = 12, units = 'in', dpi = 600, bg = 'white')

# without data points ----
p_hr <-
  ggplot(preds) +
  facet_wrap(~ group) +
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
  facet_wrap(~ group) +
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
  facet_wrap(~ group) +
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

plot_grid(p_hr, p_diff, p_exc, labels = 'auto', ncol = 1)

ggsave('figures/hgam-figure-without-T_169.png', width = 16, height = 12, units = 'in',
       dpi = 600, bg = 'white')
