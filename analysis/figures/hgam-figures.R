library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('ctmm')    # for calculating excursivity
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('mgcv')    # for GAMs
library('gratia')  # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

# import data ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  filter(! grepl('T_169', animal)) %>%
  select(! c(hr_lwr_95, hr_upr_95)) %>%
  mutate(doy = lubridate::yday(date),
         group = factor(group),
         animal_year = paste(animal, lubridate::year(date)) %>%
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
         group = stringr::str_to_sentence(group) %>%
           factor(),
         doy = lubridate::yday(timestamp),
         date = as.Date(timestamp)) %>%
  group_by(group, animal, doy, date) %>%
  summarize(excursivity = mean(excursivity)) %>%
  ungroup() %>%
  mutate(animal_year = factor(paste(animal, lubridate::year(date))))

# import models ----
m_hr <- readRDS('models/m-hr-without-T_169.rds')
m_diff <- readRDS('models/m-diff-without-T_169.rds')
m_exc <- readRDS('models/m-exc-without-T_169.rds')

# make predictions ----
newd <- expand_grid(group = unique(m_hr$model$group),
                    doy = seq(1, 365, length.out = 400),
                    animal_year = 'new animal')

get_preds <- function(parameter) {
  m <- get(paste0('m_', parameter))
  
  pr <- predict(m, newdata = newd, type = 'link', se.fit = TRUE,
                exclude = c('s(doy,animal_year):groupControl',
                            's(doy,animal_year):groupOvariectomy'),
                discrete = FALSE) %>%
    as.data.frame() %>%
    transmute(mu = inv_link(m)(fit),
              lwr = inv_link(m)(fit - se.fit * 1.96),
              upr = inv_link(m)(fit + se.fit * 1.96))
  
  colnames(pr) <- paste0(parameter, '_', colnames(pr))
  
  return(pr)
}

preds <- bind_cols(newd,
                   get_preds(parameter = 'hr'),
                   get_preds(parameter = 'diff'),
                   get_preds(parameter = 'exc'))

# make figures ----
doy_breaks <- c(1, 91, 182, 274)
doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%B %d')

#' `coplot::get_legend()` fails (v. 1.1.3.9000)
get_legend <- function(.plot) {
  get_plot_component(.plot + theme(legend.position = 'top'),
                     pattern = 'guide-box-top', return_all = TRUE)
}

theme_set(theme_get() +
            theme(plot.margin = unit(c(5.5, 20, 5.5, 5.5), units = 'pt')))

# with data points
p_hr <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_point(aes(doy, hr_est_95), mw, alpha = 0.3) +
  geom_ribbon(aes(doy, ymin = hr_lwr, ymax = hr_upr, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, hr_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('7-day home-range size (km\U00B2)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_diff <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_point(aes(doy, diffusion_km2_day), mw, alpha = 0.3) +
  geom_ribbon(aes(doy, ymin = diff_lwr, ymax = diff_upr, fill = group),
              alpha = 0.3) +
  geom_line(aes(doy, diff_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Daily diffusion rate (km\U00B2/day)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_exc <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_point(aes(doy, excursivity), d, alpha = 0.3) +
  geom_ribbon(aes(doy, ymin = exc_lwr, ymax = exc_upr, fill = group),
              alpha = 0.3) +
  geom_line(aes(doy, exc_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  scale_y_continuous('Daily excursivity', limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

plot_grid(p_hr, p_diff, p_exc, labels = 'AUTO', ncol = 1)

ggsave('figures/hgam-figure-with-data.png', width = 8, height = 10, units = 'in',
       dpi = 600, bg = 'white')

# without data points
p_hr <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_ribbon(aes(doy, ymin = hr_lwr, ymax = hr_upr, fill = group),
              alpha = 0.2) +
  geom_line(aes(doy, hr_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('7-day home-range size (km\U00B2)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_diff <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_ribbon(aes(doy, ymin = diff_lwr, ymax = diff_upr, fill = group),
              alpha = 0.3) +
  geom_line(aes(doy, diff_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab('Daily diffusion rate (km\U00B2/day)') +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

p_exc <-
  ggplot(preds) +
  facet_grid(. ~ group) +
  geom_ribbon(aes(doy, ymin = exc_lwr, ymax = exc_upr, fill = group),
              alpha = 0.3) +
  geom_line(aes(doy, exc_mu, color = group), linewidth = 1) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  scale_y_continuous('Daily excursivity', limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'none')

plot_grid(p_hr, p_diff, p_exc, labels = 'AUTO', ncol = 1)

ggsave('figures/hgam-figure.png', width = 8, height = 10, units = 'in',
       dpi = 600, bg = 'white')
