library('dplyr')   # for data wrangling
library('purrr')   # for functional programming
library('furrr')   # for parallelized functional programming
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('ctmm')    # for movement modeling
library('sf')      # for working with spatial data
source('functions/window_ctmm.R') # to calculate HRs and create figures
source('analysis/figures/default-ggplot-theme.R')

d <- readRDS('data/cleaned-telemetry-data.rds')

# number of locations
nrow(d)

# median sampling interval
medians <- d %>%
  unnest(tel) %>%
  group_by(animal) %>%
  summarise(dt = median(diff(timestamp))) %>%
  pull(dt)
median(medians)
range(medians)

# set up computational plan for increased efficiency
NCORES <- availableCores(logical = FALSE) - 2
plan(multisession, workers = NCORES)

# save movement models and UDs for full datasets ----
# necessary to estimate excursivity
m <- d %>%
  ungroup() %>%
  mutate(
    tel = future_map(tel, \(.t) as.telemetry(.t, mark.rm = TRUE)),
    vg = future_map(tel, \(.t) ctmm.guess(.t, interactive = FALSE),
                    .options = furrr_options(seed = TRUE),
                    .progress = TRUE),
    mm = future_map2(tel, vg, \(.t, .v) ctmm.select(.t, .v),
                     .options = furrr_options(seed = TRUE),
                     .progress = TRUE),
    ud = future_map2(tel, mm, \(.t, .m) akde(.t, .m, weights = TRUE),
                     .options = furrr_options(seed = TRUE),
                     .progress = TRUE))
saveRDS(m, 'models/full-telemetry-movement-models.rds')

tau_ps <- 'days' %#% map_dbl(m$mm, \(.m) {
  summary(.m, units = FALSE)$CI['τ[position] (seconds)', 'est']
})
hist(tau_ps)
mean(tau_ps)
sd(tau_ps)

range(tau_ps[- which.max(tau_ps)])

# fit moving window models and UDs ----
if(! dir.exists('figures/moving-windows')) dir.create('figures/moving-windows')
if(! dir.exists('models/moving-windows')) dir.create('models/moving-windows')

# fitting moving window models using multiple parallel sessions
START <- Sys.time(); START
tictoc::tic()
d %>%
  unnest(tel) %>%
  as.telemetry(mark.rm = TRUE) %>%
  future_map(function(.d) {
    window_ctmm(
      .d,
      window = 7 %#% 'day', # each window is 7 days in size
      dt = 3 %#% 'day', # shift window by 3 days each time
      fig_path = 'figures/moving-windows',
      rds_path = 'models/moving-windows',
      cores = 1, # cannot parallelize on Windows
      akde_weights = TRUE, # weigh points by interval between locations
      progress = 0) # level of progress tracking
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
tictoc::toc()
END <- Sys.time(); END

# check parameters for each model to get preliminary results ----
models <- map_dfr(list.files('models/moving-windows',
                             pattern = '-window-7-days-dt-3-days.rds',
                             full.names = TRUE), readRDS) %>%
  mutate(group = tolower(group))

# find proportion of windows with tau_p < 1 day
mean(models$tau_p_hours < 24, na.rm = TRUE)
sum(models$tau_p_hours > 24 * 3, na.rm = TRUE)
max(models$tau_p_hours, na.rm = TRUE) / 24
max(filter(models, animal != 'T_169')$tau_p_hours, na.rm = TRUE) / 24

# find counts and proportions of windows with tau_p and tau_v
totals <- models %>%
  group_by(group) %>%
  summarize(insuff_data = sum(map_lgl(guess, \(.g) class(.g) == 'character')),
            iid = map_chr(model, \(.m) {
              if(class(.m) == 'ctmm') {
                summary(.m)$name
              } else {
                'Insufficient data.'
              }
              }) %>%
              grepl('IID', .) %>%
              sum(),
            has_hr = sum(! is.na(hr_est_95)),
            has_tau_p = sum(! is.na(tau_p_hours)),
            has_tau_v = sum(! is.na(tau_v_hours)),
            has_diffusion = sum(! is.na(diffusion_km2_day)),
            total = n()) %>%
  bind_rows(.,
            bind_cols(group = 'Total', t(colSums(select(., -1)))))
totals

# find proportions
totals %>%
  mutate(across(insuff_data:total, \(.col) .col / total))

# figures of tau_p, tau_v, and diffusion
plot_grid(
  ggplot(models, aes(tau_p_hours)) +
    facet_grid(. ~ group) +
    geom_histogram(center = 0.5, binwidth = 1) +
    labs(x = 'Range crossing time (hours)', y = 'Count'),
  models %>%
    mutate(tau_v_hours = if_else(is.na(tau_v_hours), -0.1, tau_v_hours)) %>%
    ggplot(aes(tau_v_hours, fill = tau_v_hours < 0)) +
    facet_grid(. ~ group) +
    geom_histogram(center = 0.025, binwidth = 0.05) +
    labs(x = 'Directional persistence (hours)', y = 'Count') +
    scale_fill_manual(expression(bold(No~finite~hat(tau)[v])),
                      values = c('grey30', 'red3')) +
    theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.7)),
  models %>%
    mutate(diffusion_km2_day = if_else(is.na(diffusion_km2_day),
                                       -0.01, diffusion_km2_day)) %>%
    ggplot(aes(diffusion_km2_day, fill = diffusion_km2_day < 0)) +
    facet_grid(. ~ group) +
    geom_histogram(center = 0.25, binwidth = 0.5) +
    labs(x = expression(bold(Diffusion~(km^2/day))), y = 'Count') +
    scale_fill_manual(expression(bold(No~finite~diffusion~estimate)),
                      values = c('grey30', 'red3')) +
    theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.7)),
  ncol = 1)

models <- filter(models, ! map_lgl(akde, is.null))

uds <- lapply(models$akde, SpatialPolygonsDataFrame.UD) %>%
  do.call("rbind", .) %>%
  st_as_sf() %>%
  filter(grepl('est', name))

uds <- mutate(st_geometry(uds) %>%
                st_as_sf(),
              large = as.numeric(st_area(uds)) > 10 %#% 'km^2',
              group = if_else(grepl('C', uds$name), 'control', 'ovariectomy'))

plot_grid(ggplot(uds) +
            facet_wrap(~ group) +
            geom_sf(aes(color = group), fill = 'transparent', lwd = 1) +
            scale_color_manual('Group', values = paste0(PAL, '10')) +
            theme(legend.position = 'none'),
          ggplot(uds) +
            facet_wrap(~ group) +
            geom_sf(aes(color = large), fill = 'transparent', lwd = 1) +
            scale_color_manual(expression(bold(HR~'> 10 km'^'2')),
                               values = c('black', 'red2')))

# check missing windows ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  mutate(group = factor(if_else(group == 'Ovariectomy', 'Treatment', group)),
         animal = factor(animal)) %>%
  select(group, animal, date, hr_est_95) %>%
  filter(! is.na(hr_est_95))

d_ref <- readr::read_csv('data/Odocoileus virginianus DeNicola South Euclid-reference-data.csv',
                  show_col_types = FALSE, col_types = c(`animal-sex` = 'c')) %>%
  rename_with(\(.names) gsub('-', '_', .names))

# there are missing windows
mw %>%
  group_by(animal) %>%
  transmute(dt = c(0, diff(date))) %>%
  pull(dt) %>%
  unique()

# create dotplot of moving windows
d_ref <- mutate(d_ref, animal_id = factor(animal_id))

mw %>%
  mutate(animal = as.character(animal)) %>%
  ggplot() +
  geom_hline(aes(yintercept = animal_id), d_ref, color = 'grey') +
  geom_point(aes(date, animal, color = group), pch = 20) +
  geom_point(aes(deploy_on_date, animal_id, shape = 'start'), d_ref,
             size = 5) +
  geom_point(aes(deploy_off_date, animal_id, shape = 'end'), d_ref,
             size = 5, na.rm = TRUE) +
  geom_point(aes(animal_mortality_date, animal_id, shape = 'mortality'),
             d_ref, size = 7, na.rm = TRUE) +
  scale_color_manual('Group', values = PAL) +
  scale_shape_manual('Event type', breaks = c('start', 'end', 'mortality'),
                     values = c('[', ']', '\U00d7')) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = NULL, y = 'Animal ID') +
  theme(legend.position = 'top')

ggsave('figures/moving-window-dotplot.png', width = 10, height = 8,
       units = 'in', dpi = 600, bg = 'white')
