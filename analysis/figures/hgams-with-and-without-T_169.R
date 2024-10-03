library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('mgcv')    # for GAMs
library('gratia')  # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

models <- expand_grid(parameter = c('hr', 'diff', 'exc'),
                      with_T_169 = c('With T_169', 'Without T_169') %>%
                        factor(levels = c('With T_169', 'Without T_169'))) %>%
  mutate(file_name = paste0('models/m-', parameter,
                            if_else(with_T_169 == 'With T_169',
                                    '-with-T_169.rds',
                                    '-without-T_169.rds')),
         model = map(file_name, readRDS))

# figure of intercepts ----
intercepts <-
  models %>%
  mutate(int = map(model, \(.m) {
    ilink <- inv_link(.m)
    
    tibble(group = c('Control', 'Ovariectomy'),
           doy = 0,
           doy_cr = 0,
           animal_year = 'new') %>%
      bind_cols(.,
                predict.bam(.m, newdata = ., type = 'link', se.fit = TRUE,
                            terms = c('group'),
                            discrete = FALSE) %>%
                  as.data.frame()) %>%
      mutate(fit = fit + coef(.m)['(Intercept)'],
             lwr = ilink(fit - 1.96 * se.fit),
             mu = ilink(fit),
             upr = ilink(fit + 1.96 * se.fit))
  })) %>%
  select(! c(file_name, model)) %>%
  unnest(int) %>%
  mutate(parameter = case_when(
    parameter == 'hr' ~ 'Home-range size (km\U00B2)',
    parameter == 'diff' ~ 'Diffusion (km\U00B2/day)',
    parameter == 'exc' ~ 'Excursivity') %>%
      factor(., levels = c('Home-range size (km\U00B2)',
                           'Diffusion (km\U00B2/day)',
                           'Excursivity')),
    group = if_else(group == 'Ovariectomy', 'Treatment' , group))

unique(last_dplyr_warnings())

p_intercepts <-
  ggplot(intercepts) +
  facet_grid(parameter ~ with_T_169, scales = 'free_y', switch = 'y') +
  geom_hline(aes(yintercept = mu), filter(intercepts, group == 'Control'),
             color = 'grey') +
  geom_errorbar(aes(group, ymin = lwr, ymax = upr, width = 0)) +
  geom_point(aes(group, mu)) +
  labs(x = NULL, y = NULL) +
  theme(strip.placement = 'outside', strip.background.y = element_blank(),
        strip.text.y = element_text(size = 11))

# figure of smooth partial effects ----
partials <- models %>%
  transmute(parameter,
            with_T_169,
            part = map(model, smooth_estimates)) %>%
  unnest(part) %>%
  rename_with(\(x) gsub('\\.', '', x)) %>%
  rename(term = smooth) %>%
  mutate(doy = if_else(is.na(doy), doy_cr, doy),
         lwr = estimate - 1.96 * se,
         mu = estimate,
         upr = estimate + 1.96 * se,
         term = if_else(type == 'Factor smooth',
                        'Individual-level deviations',
                        'Population mean') %>%
           factor(., levels = unique(.)))

plot_partials <- function(param) {
  doy_breaks <- c(1, 101, 201, 301)
  doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%b %d')
  
  # if(param != 'exc') {
  #   g_inv <- function(eta) pmax(exp(eta), .Machine$double.eps)
  # } else {
  #   g_inv <- function(eta) .Call(C_logit_linkinv, eta)
  # }
  
  partials %>%
    filter(parameter == param, term != 'Intercept') %>%
    filter(! (group == 'Control' & grepl('T_', animal_year) |
                group == 'Ovariectomy' & grepl('C_', animal_year))) %>%
    mutate(#across(c(lwr, mu, upr), g_inv),
      g = case_when(is.na(animal_year) ~ 'pop',
                     grepl('T_169', animal_year) ~ 'T_169',
                     ! is.na(animal_year) ~ 're'),
      group = if_else(group == 'Ovariectomy', 'Treatment', 'Control')) %>%
    ggplot(aes(doy, mu)) +
    facet_grid(term + group ~ with_T_169, scales = 'free_y') +
    geom_hline(yintercept = 0, color = 'grey') +
    geom_ribbon(aes(ymin = lwr, ymax = upr, group = animal_year,
                    fill = g), alpha = 0.1, show.legend = FALSE) +
    geom_line(aes(group = animal_year, alpha = g == 're',
                  lwd = g != 'pop', color = g == 'T_169'),
              show.legend = FALSE) +
    scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                       expand = c(0, 0)) +
    ylab('Estimated smooth partial effect') +
    scale_color_manual(values = c('black', 'red3')) +
    scale_fill_manual(values = c(pop = 'black', re = 'grey75',
                                 T_169 = 'red3')) +
    scale_alpha_manual(values = c(1, 0.3)) +
    scale_linewidth_manual(values = c(1, 0.5))
}

# figure of partial effects ----
# HGAM fits with and without doe T_169. Panel A shows the estimated
# averages for the control and the treatment with 95% CIs for the
# difference. Panels B-D show the estimated smooth partial effects for each
# model on the link scale. Doe T_169 is indicated in red.
p_effects <- plot_grid(p_intercepts,
                       plot_partials(param = 'hr'),
                       plot_partials(param = 'diff'),
                       plot_partials(param = 'exc'),
                       labels = 'AUTO')

ggsave('figures/partial-effects.png', p_effects, width = 16, height = 16,
       units = 'in', dpi = 600, bg = 'white')

# make figure of model diagnostics ---
# Model diagnostics for home-range size (A-B), diffusion (C-D), and
# excursivity (E-F) with doe T_169 (left column) and without (right
# column).
p_diag <- plot_grid(
  appraise(models$model[[1]], point_alpha = 0.1),
  appraise(models$model[[2]], point_alpha = 0.1),
  NULL, NULL,
  appraise(models$model[[3]], point_alpha = 0.1),
  appraise(models$model[[4]], point_alpha = 0.1),
  NULL, NULL,
  appraise(models$model[[5]], type = 'pearson', point_alpha = 0.1),
  appraise(models$model[[6]], type = 'pearson', point_alpha = 0.1),
  nrow = 2, byrow = FALSE, rel_widths = c(10, 1, 10, 1, 10),
  labels = c('A', '', 'B', '', 'C', 'D', '', 'E', '', 'F'))

ggsave('figures/model-diagnostics.png', p_diag, width = 12, height = 8,
       units = 'in', dpi = 600, bg = 'white', scale = 2)
