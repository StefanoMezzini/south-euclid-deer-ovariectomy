library('dplyr')   # for data wrangling
library('purrr')   # for functional programming
library('furrr')   # for parallelized functional programming
library('tidyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('ctmm')    # for movement modeling
source('functions/window_ctmm.R') # to calculate HRs and create figures
source('analysis/figures/default-ggplot-theme.R')

d <- readRDS('data/cleaned-telemetry-data.rds')

if(! dir.exists('figures/moving-windows')) dir.create('figures/moving-windows')
if(! dir.exists('models/moving-windows')) dir.create('models/moving-windows')

# fitting moving window models using multiple parallel sessions
plan(multisession(workers = 10))
START <- Sys.time(); START
tictoc::tic()
d %>%
  unnest(tel) %>%
  as.telemetry(keep = TRUE, mark.rm = TRUE) %>%
  future_map(function(.d) {
    # cat('Running', .d@info$identity, '\n')
    window_ctmm(
      .d,
      window = 7 %#% 'day', # each window is 30 days in size
      dt = 3 %#% 'day', # shift window by 10 days each time
      fig_path = 'figures/moving-windows',
      rds_path = 'models/moving-windows',
      cores = 1, # cannot parallelize on Windows
      akde_weights = TRUE, # weigh points by interval between locations
      progress = 0) # level of progress tracking
  }, .options = furrr_options(seed = FALSE), .progress = TRUE)
tictoc::toc()
END <- Sys.time(); END

# check parameters for each model ----
models <- map_dfr(list.files('models/moving-windows',
                             pattern = '-window-7-days-dt-3-days.rds',
                             full.names = TRUE), readRDS)

models %>%
  group_by(group) %>%
  summarize(has_hr = sum(! is.na(hr_est_95)),
            has_tau_p = sum(! is.na(tau_p_hours)),
            has_tau_v = sum(! is.na(tau_v_hours)),
            has_diffusion = sum(! is.na(diffusion_km2_day)),
            total = n())

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
    theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.8)),
  models %>%
    mutate(diffusion_km2_day = if_else(is.na(diffusion_km2_day),
                                       -0.01, diffusion_km2_day)) %>%
    ggplot(aes(diffusion_km2_day, fill = diffusion_km2_day < 0)) +
    facet_grid(. ~ group) +
    geom_histogram(center = 0.25, binwidth = 0.5) +
    labs(x = expression(bold(Diffusion~(km^2/day))), y = 'Count') +
    scale_fill_manual(expression(bold(No~finite~diffusion~estimate)),
                      values = c('grey30', 'red3')) +
    theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.8)),
  ncol = 1)

ggplot(models, aes(posixct, hr_est_95)) +
  facet_grid(. ~ group) +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = animal), method = 'gam', formula = y ~ s(x, k = 5)) +
  scale_x_datetime() +
  ylim(c(0, 10))


