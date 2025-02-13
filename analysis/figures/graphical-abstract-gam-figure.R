library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('ggplot2')   # for fancy plots
library('mgcv')      # for GAMs
library('gratia')    #' for `inv_link()`
source('analysis/figures/default-ggplot-theme.R')

# import models ----
m_hr <- readRDS('models/m-hr.rds')
m_diff <- readRDS('models/m-diff.rds')
m_exc <- readRDS('models/m-exc.rds')

doy_breaks <- c(1, 91, 182, 274)
doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%B 1')

# make predictions ----
newd <- expand_grid(group = unique(m_hr$model$group),
                    doy = seq(1, 365, length.out = 400),
                    doy_cr = 0,
                    animal_year = 'new animal')

#' unlike `functions/get_preds.R`, adds the data a column of parameter
get_preds <- function(parameter) {
  m <- get(paste0('m_', parameter))
  
  predict(m, newdata = newd, type = 'link', se.fit = TRUE,
          exclude = c('s(doy_cr,animal_year):groupControl',
                      's(doy_cr,animal_year):groupOvariectomy'),
          # Smoothness uncertainty corrected covariance not available
          discrete = FALSE, unconditional = FALSE) %>%
    as.data.frame() %>%
    transmute(mu = inv_link(m)(fit),
              lwr_50 = inv_link(m)(fit - se.fit * 0.67),
              upr_50 = inv_link(m)(fit + se.fit * 0.67),
              lwr_95 = inv_link(m)(fit - se.fit * 1.96),
              upr_95 = inv_link(m)(fit + se.fit * 1.96)) %>%
    mutate(metric = parameter) %>%
    bind_cols(newd, .) %>%
    return()
}

# make figures ----
bind_rows(get_preds(parameter = 'hr'),
          get_preds(parameter = 'diff'),
          get_preds(parameter = 'exc')) %>%
  mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group)) %>%
  mutate(
    lab = case_when(metric == 'hr' ~ '7-day home-range size (km\U00B2)',
                    metric == 'diff' ~ 'Daily diffusion rate (km\U00B2/day)',
                    metric == 'exc' ~ 'Daily excursivity')) %>%
  ggplot(aes(doy)) +
  facet_grid(lab ~ ., scales = 'free', switch = 'y') +
  geom_ribbon(aes(ymin = lwr_95, ymax = upr_95, fill = group), alpha = 0.2) +
  geom_ribbon(aes(ymin = lwr_50, ymax = upr_50, fill = group), alpha = 0.2) +
  geom_line(aes(y = mu, color = group), linewidth = 1.5) +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0)) +
  ylab(NULL) +
  scale_fill_manual('Group', values = PAL, aesthetics = c('color', 'fill')) +
  theme(legend.position = 'top', strip.placement.y = 'outside',
        strip.background.y = element_blank(),
        strip.text.y = element_text(size = 11))

ggsave('figures/graphical-abstract-gam-figure.png',
       width = 6, height = 8, dpi = 600, bg = 'white')
