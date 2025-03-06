library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for funtional programming
library('ggplot2') # for fancy plots
library('ggmap')   # for basemaps
library('cowplot') # for fancy multi-panel plots
library('sf')      # for spatial data (UD polygons)
library('mgcv')    # for GAMs
library('gratia')  # for ggplot-based model plots
source('analysis/figures/default-ggplot-theme.R')

# moving window data ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  select(! c(hr_lwr_95, hr_upr_95)) %>%
  mutate(doy = lubridate::yday(date),
         doy_cr = doy,
         group = factor(group),
         animal = factor(animal),
         speed_m_s = map_dbl(model, \(.m) {
           
           if(is.null(.m)) {
             s <- NA_real_ 
           } else {
             s <- speed(.m, units = FALSE)$CI[, 'est']
           }
           
           if(is.infinite(s)) {
             s <- NA_real_
           }
           return(s)
         }),
         animal_year = paste(animal, lubridate::year(date)) %>%
           factor())

unique(last_dplyr_warnings())[[1]]

# get stats for range crossing times
median(mw$tau_p_hours, na.rm = T)
mean(mw$tau_p_hours, na.rm = T)
sd(mw$tau_p_hours, na.rm = T)
mean(mw$tau_p_hours < 24, na.rm = TRUE)
sum(mw$tau_p_hours > 24 * 3, na.rm = TRUE)

# proportions across groups
mw %>%
  filter(map_lgl(guess, \(.g) class(.g) != 'character')) %>%
  summarize(iid = map_chr(model, \(.m) {
    if(class(.m) == 'ctmm') {
      summary(.m)$name
    } else {
      'Insufficient data.'
    }
  }) %>%
    grepl('IID', .) %>%
    mean(),
  has_hr = mean(! is.na(hr_est_95)),
  has_tau_p = mean(! is.na(tau_p_hours)),
  has_tau_v = mean(! is.na(tau_v_hours)),
  has_diffusion = mean(! is.na(diffusion_km2_day)),
  total = n())

# table S2
# find percentage and n of windows with HR and diffusion estimates
mw %>%
  group_by(group) %>%
  summarize(
    hr = sum(! is.na(hr_est_95) & hr_est_95 < 10),
    diff = sum(! is.na(diffusion_km2_day) & diffusion_km2_day < 2),
    exc = sum(map_int(dataset, \(.tel) nrow(.tel)) > 0),
    n = n(),
    .groups = 'drop') %>%
  bind_rows(.,
            bind_cols(group = 'Total', t(colSums(select(., -1))))) %>%
  mutate(p_hr = paste0(round(hr / n * 100, 1), '%'),
         p_diff = paste0(round(diff / n * 100, 1), '%'),
         p_exc = paste0(round(exc / n * 100, 1), '%'))

# percentage of windows with <= 5 locations (so no movement model)
mw %>%
  group_by(group) %>%
  mutate(n_obs = map(dataset, nrow)) %>%
  summarise(n_5 = sum(n_obs <= 5), # should be == n(character) below
            no_guess = sum(map_lgl(guess, \(.g) class(.g) == 'character')),
            total = n()) %>%
  mutate(perc = n_5 / total * 100)

# find widows with at least 3 edf for HR and diffusion ----
mw <- mutate(mw,
             hr_edf = map_dbl(model, \(.m) {
               if(class(.m) == 'ctmm') {
                 return(summary(.m)$DOF['area'])
               } else {
                 return(NA_real_)
               }
             }),
             diff_edf = map_dbl(model, \(.m) {
               if(class(.m) == 'ctmm') {
                 return(summary(.m)$DOF['diffusion'])
               } else {
                 return(NA_real_)
               }
             }))

# hr
range(mw$hr_edf, na.rm = TRUE)
mean(mw$hr_edf > 3, na.rm = TRUE)
sum(mw$hr_edf > 3, na.rm = TRUE)
sum(! is.na(mw$hr_edf))

# diffusion
range(mw$diff_edf, na.rm = TRUE)
mean(mw$diff_edf > 3, na.rm = TRUE)
sum(mw$diff_edf > 3, na.rm = TRUE)
sum(! is.na(mw$diff_edf))

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
         group = stringr::str_to_sentence(group) %>%
           factor(),
         doy = lubridate::yday(timestamp),
         doy_cr = doy,
         date = as.Date(timestamp)) %>%
  group_by(group, animal, doy, doy_cr, date) %>%
  summarize(excursivity = mean(excursivity)) %>%
  ungroup() %>%
  mutate(animal_year = factor(paste(animal, lubridate::year(date))))

ggplot(d, aes(doy, excursivity, group = animal_year)) +
  facet_wrap(~ group, ncol = 1) +
  geom_point() +
  geom_line() +
  geom_smooth(color = 'darkorange', method = 'gam', formula = y ~ s(x),
              method.args = list(family = betar()))

#' 9 of the does were tracked in both years (need to use `animal_year`)
mw %>%
  group_by(animal) %>%
  summarize(n = n_distinct(animal_year)) %>%
  filter(n == 2)

# not enough speed estimates to do anything meaningful
mw %>%
  group_by(group) %>%
  summarize(has_speed = sum(! is.na(speed_m_s)),
            total = n(),
            perc = has_speed / total * 100)

ggplot(mw, aes(posixct, speed_m_s, group = animal)) +
  facet_wrap(~ group, ncol = 1) +
  geom_line() +
  geom_point(alpha = 0.3)

# figure comparing parameters of T_169 to others ----
# will drop HRs > 12 km^2
plot_grid(
  mw %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group),
           col = case_when(animal == 'T_158' ~ 'Deer T_158',
                           animal == 'T_169' ~ 'Deer T_169',
                           TRUE ~ 'Other deer')) %>%
    ggplot(aes(posixct, hr_est_95, color = col)) +
    facet_wrap(~ group) +
    geom_hline(yintercept = 10, linetype = 'dashed') +
    geom_line(aes(group = animal)) +
    geom_point() +
    labs(x = NULL, y = expression(bold('Home-range'~size~(km^2)))) +
    scale_color_manual(NULL, values = c('#00008B80', '#CD000080', '#00000016')) +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.92, 0.8)),
  mw %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group),
           col = case_when(animal == 'T_158' ~ 'Deer T_158',
                           animal == 'T_169' ~ 'Deer T_169',
                           TRUE ~ 'Other deer')) %>%
    ggplot(aes(posixct, diffusion_km2_day, color = col)) +
    facet_wrap(~ group) +
    geom_hline(yintercept = 2, linetype = 'dashed') +
    geom_line(aes(group = animal)) +
    geom_point() +
    labs(x = NULL, y = expression(bold(Diffusion~(km^2/day)))) +
    scale_color_manual(NULL, values = c('#00008B80', '#CD000080', '#00000016')) +
    theme(legend.position = 'none'),
  d %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group),
           col = case_when(animal == 'T_158' ~ 'Deer T_158',
                           animal == 'T_169' ~ 'Deer T_169',
                           TRUE ~ 'Other deer')) %>%
    ggplot(aes(date, excursivity, group = animal, color = col)) +
    facet_wrap(~ group) +
    geom_point() +
    geom_line() +
    labs(x = NULL, y = 'Excursivity') +
    scale_color_manual(NULL, values = c('#00008B80', '#CD000080', '#00000016')) +
    theme(legend.position = 'none'),
  ncol = 1, labels = 'auto')
ggsave('figures/deer-T_169-comparison.png', width = 10, height = 8)

# make figures of full UDs ----
mm <- readRDS('models/full-telemetry-movement-models.rds')
T_169 <- which(mm$animal == 'T_169')

uds <- imap_dfr(mm$animal, \(.a, .i) {
  SpatialPolygonsDataFrame.UD(mm$ud[[.i]]) %>%                              
    st_as_sf() %>%
    st_transform(crs = 'EPSG:4326') %>%
    as_tibble() %>%
    slice(n():1) %>%
    mutate(animal = .a)
}) %>%
  mutate(est = grepl('est', name))

ud_169 <- filter(uds, animal == 'T_169')

tel_169 <- SpatialPointsDataFrame.telemetry(mm$tel[[T_169]]) %>%
  st_as_sf() %>%
  st_transform('EPSG:4326') %>%
  st_coordinates() %>%
  as_tibble()

# basemap
if(file.exists('data/south-euclid-basemap.rds')) {
  bm <- readRDS('data/south-euclid-basemap.rds')
} else {
  bm <- bind_rows(stations, tels) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_as_sf() %>%
    st_buffer(1e3) %>%
    st_bbox() %>%
    `names<-`(c('left', 'bottom', 'right', 'top')) %>%
    get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 14)
  saveRDS(bm, 'data/south-euclid-basemap.rds')
}

plot_grid(
  ggmap(bm) +
    geom_sf(aes(geometry = geometry, lwd = est), ud_169,
            fill = '#00000017', show.legend = FALSE, inherit.aes = FALSE) +
    geom_point(aes(X, Y), tel_169, size = 0.5, inherit.aes = FALSE) +
    geom_path(aes(X, Y), tel_169, alpha = 0.2, inherit.aes = FALSE) +
    labs(x = NULL, y = NULL) +
    scale_linewidth_manual(values = c(0.25, 0.75)),
  ggmap(bm) +
    geom_sf(aes(geometry = geometry, lwd = est, color = animal != 'T_169'),
            uds, fill = '#00000017', show.legend = FALSE,
            inherit.aes = FALSE) +
    labs(x = NULL, y = NULL) +
    scale_linewidth_manual(values = c(0.25, 0.75)) +
    scale_color_manual(NULL, values = c('red3', 'black'),
                       labels = c('Deer T_169', 'Other deer')),
  labels = 'auto')

ggsave('figures/deer-T_169-comparison-ud.png', width = 10, height = 8,
       bg = 'white')

# find n and percentages of data dropped ----
# hr
sum(mw$hr_est_95 > 10, na.rm = TRUE)
paste0(round(mean(mw$hr_est_95 > 10, na.rm = TRUE) * 100, 2), '%')

# diffusion
sum(mw$diffusion_km2_day > 2, na.rm = TRUE)
paste0(round(mean(mw$diffusion_km2_day > 2, na.rm = TRUE) * 100, 2), '%')

# figure 
mw %>%
  filter(hr_est_95 > 10 | diffusion_km2_day > 2) %>%
  transmute(hr_est_95, diffusion_km2_day,
            t_start = as.POSIXct(t_start) %>% as.Date(),
            t_end = as.POSIXct(t_end) %>% as.Date(),
            dataset = map(dataset, data.frame),
            id = paste0('Deer ', animal,
                        '\n', t_start, ' to ', t_end),
            drop = case_when(
              hr_est_95 > 10 & diffusion_km2_day > 2 ~ 'Home range and diffusion',
              hr_est_95 > 10 ~ 'Home range',
              diffusion_km2_day > 2 ~ 'Diffusion')) %>%
  unnest(dataset) %>%
  ggplot() +
  coord_equal() +
  facet_wrap(~ id, nrow = 2) +
  geom_path(aes(longitude, latitude, color = drop)) +
  geom_point(aes(longitude, latitude, color = drop), alpha = 0.3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_color_brewer('Dropped', type = 'qual', palette = 2) +
  theme(legend.position = 'inside',
        legend.position.inside = c(8.6/9, 1/4))
ggsave('figures/values-dropped-telemetries.png', width = 8, height = 5,
       scale = 2, bg = 'white')

# home range size ----
m_hr <- bam(
  hr_est_95 ~
    group + #' `by` requires an explicit intercept for each group
    s(doy, by = group, k = 10, bs = 'cc') +
    s(doy_cr, by = group, animal_year, k = 10, bs = 'fs',
      xt = list(bs = 'cr')),
  family = Gamma('log'),
  data = mw,
  subset = hr_est_95 < 10, # drop unrealistic HR estimates
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)))

appraise(m_hr, point_alpha = 0.1)
draw(m_hr, nrow = 3, parametric = TRUE, residuals = TRUE)
saveRDS(m_hr, file = 'models/m-hr.rds')

# diffusion ----
m_diff <- bam(
  diffusion_km2_day ~
    group + #' `by` requires an explicit intercept for each group
    s(doy, by = group, k = 10, bs = 'cc') +
    s(doy_cr, by = group, animal_year, k = 10, bs = 'fs',
      xt = list(bs = 'cr')),
  family = Gamma('log'),
  data = mw,
  subset = hr_est_95 < 2, # drop outlier diffusion estimate
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)))

appraise(m_diff, point_alpha = 0.1)
draw(m_diff, nrow = 3, parametric = TRUE, residuals = TRUE)
saveRDS(m_diff, file = 'models/m-diff.rds')

# excursivity (fits in ~ 160 seconds) ----
m_exc <- bam(
  excursivity ~
    group + #' `by` requires an explicit intercept for each group
    s(doy, by = group, k = 10, bs = 'cc') +
    s(doy_cr, by = group, animal_year, k = 10, bs = 'fs',
      xt = list(bs = 'cr')),
  family = betar(link = 'logit'),
  data = d,
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)),
  control = gam.control(trace = TRUE))

appraise(m_exc, type = 'pearson', point_alpha = 0.1)
draw(m_exc, parametric = TRUE, residuals = TRUE)
saveRDS(m_exc, file = 'models/m-exc-without-T_169.rds')

# plot predicted vs observed
ggplot(d, aes(fitted(m_exc), excursivity)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'darkorange') +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 20))

# check dev.expl and scale parameters
tibble(parameter = c('hr', 'diffusion', 'excursivity'),
       model = list(readRDS('models/m-hr.rds'),
                    readRDS('models/m-diff.rds'),
                    readRDS('models/m-exc.rds')),
       dev_expl = purrr::map_dbl(model, \(.m) summary(.m)$dev.expl) * 100,
       scale_est = purrr::map_dbl(model, \(.m) summary(.m)$scale))
