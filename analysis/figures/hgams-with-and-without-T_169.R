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
                                    '.rds',
                                    '-without-T_169.rds')),
         model = map(file_name, readRDS))

# breaks for day of year x axis
doy_breaks <- c(1, 101, 201, 301)
doy_labs <- format(as.Date('2022-12-31') + doy_breaks, '%b %d')

# figure of group-level differences ----
differences <-
  models %>%
  mutate(int = map(model, \(.m) {
    tibble(group = 'Ovariectomy',
           difference = 1,
           doy = seq(1, 366, by = 1),
           doy_cr = doy,
           animal_year = 'T_151 2023') %>%
      bind_cols(.,
                predict.bam(.m, newdata = ., type = 'link', se.fit = TRUE,
                            terms = c('(Intercept)', 's(doy):difference'),
                            discrete = FALSE) %>%
                  as.data.frame()) %>%
      mutate(fit = fit,
             lwr = fit - 1.96 * se.fit,
             upr = fit + 1.96 * se.fit)
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
    # change "Ovariectomy" to "Treatment"
    group = if_else(group == 'Ovariectomy', 'Treatment' , group))

p_differences <-
  ggplot(differences) +
  facet_grid(parameter ~ with_T_169, scales = 'fixed') +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_ribbon(aes(doy, ymin = lwr, ymax = upr), alpha = 0.1) +
  geom_line(aes(doy, fit), linewidth = 1) +
  ylab('Estimated difference (treatment minus control, on link scale)') +
  scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                     expand = c(0, 0))

# figures of smooth partial effects for each group ----
partials <- models %>%
  transmute(parameter,
            with_T_169,
            part = map(model, smooth_estimates)) %>%
  unnest(part) %>%
  # drop the "." at the beginning of the columns
  rename_with(\(x) gsub('\\.', '', x)) %>%
  rename(term = smooth) %>%
  mutate(doy = if_else(is.na(doy), doy_cr, doy), # merge doy and doy_cr
         difference = if_else(group == 'Control', 0, 1),
         lwr = estimate - 1.96 * se,
         mu = estimate,
         upr = estimate + 1.96 * se,
         term = if_else(type == 'Factor smooth',
                        'Individual-level deviations',
                        'Population mean') %>%
           factor(., levels = unique(.))) %>%
  select(! c(doy_cr, ))

plot_partials <- function(param) {
  YLAB <- case_when(
    param == 'hr' ~ 'Estimated partial effect on HR size (link scale)',
    param == 'diff' ~ 'Estimated partial effect on diffusion (link scale)',
    param == 'exc' ~ 'Estimated partial effect on excursivity (link scale)')
  
  d_ylim <- tibble(x = 1,
                   y = c(min(filter(partials, parameter == param)$lwr),
                         max(filter(partials, parameter == param)$upr)),
                   term = factor('Individual-level deviations',
                                 levels = levels(partials$term))) 
  
  partials %>%
    filter(parameter == param) %>% # filter to specific movement parameter
    # drop animals from the other group
    filter(! (group == 'Control' & grepl('T_', animal_year) |
                group == 'Ovariectomy' & grepl('C_', animal_year))) %>%
    mutate(
      # add group varaible for: population, T_169, other deer
      g = case_when(is.na(animal_year) ~ 'pop',
                    grepl('T_169', animal_year) ~ 'T_169',
                    ! is.na(animal_year) ~ 'other'),
      group = case_when(group == 'Control' ~ 'Control',
                        group == 'Ovariectomy' ~ 'Treatment',
                        by == 'difference' ~ 'Treatment',
                        is.na(by) ~ 'Control')) %>%
    ggplot(aes(doy, mu)) +
    facet_grid(term + group ~ with_T_169, scales = 'free_y') +
    # add common axes for the fs smooths
    geom_point(aes(x, y), d_ylim, color = 'transparent') +
    # add estimated lines and CIs
    geom_hline(yintercept = 0, color = 'grey') +
    geom_ribbon(aes(ymin = lwr, ymax = upr, group = animal_year,
                    fill = g), alpha = 0.1, show.legend = FALSE) +
    geom_line(aes(group = animal_year, alpha = g == 'other',
                  lwd = g != 'pop', color = g == 'T_169'),
              show.legend = FALSE) +
    scale_x_continuous(NULL, breaks = doy_breaks, labels = doy_labs,
                       expand = c(0, 0)) +
    ylab(YLAB) +
    scale_color_manual(values = c('black', 'red3')) +
    scale_fill_manual(values = c(pop = 'black', other = 'grey75',
                                 T_169 = 'red3')) +
    scale_alpha_manual(values = c(1, 0.3)) +
    scale_linewidth_manual(values = c(1, 0.5))
}

# figure of partial effects ----
# HGAM fits with and without doe T_169. Panel A shows the estimated
# averages for the control and the treatment with 95% CIs for the
# difference. Panels B-D show the estimated smooth partial effects for each
# model on the link scale. Doe T_169 is indicated in red.
p_effects <- plot_grid(
  p_differences,
  plot_partials(param = 'hr'),
  plot_partials(param = 'diff'),
  plot_partials(param = 'exc'),
  labels = 'auto')

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
  labels = c('a', '', 'b', '', 'c', 'd', '', 'e', '', 'f'))

ggsave('figures/model-diagnostics.png', p_diag, width = 12, height = 8,
       units = 'in', dpi = 600, bg = 'white', scale = 2)
