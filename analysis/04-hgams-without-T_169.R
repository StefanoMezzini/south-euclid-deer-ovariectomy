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

mw %>%
  mutate(hr_est_95 = if_else(animal == 'T_169', NA_real_, hr_est_95),
         diffusion_km2_day = if_else(animal == 'T_169', NA_real_,
                                     diffusion_km2_day)) %>%
  group_by(group) %>%
  summarize(
    hr = sum(! is.na(hr_est_95)),
    diff = sum(! is.na(diffusion_km2_day)),
    n = n(),
    .groups = 'drop') %>%
  bind_rows(.,
            bind_cols(group = 'Total', t(colSums(select(., -1))))) %>%
  mutate(hr = paste0(round(hr / n * 100, 1), '%'),
         diff = paste0(round(diff / n * 100, 1), '%'))

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

d %>%
  mutate(excursivity = if_else(animal == 'T_169', NA_real_, excursivity)) %>%
  group_by(group) %>%
  summarize(used = sum(! is.na(excursivity )), n = n(), .groups = 'drop') %>%
  bind_rows(.,
            bind_cols(group = 'Total', t(colSums(select(., -1))))) %>%
  mutate(used = paste0(round(used / n * 100, 1), '%'))

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
            total = n())

ggplot(mw, aes(posixct, speed_m_s, group = animal)) +
  facet_wrap(~ group, ncol = 1) +
  geom_line() +
  geom_point(alpha = 0.3)

# figure comparing parameters of T_169 to others ----
plot_grid(
  mw %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group)) %>%
    ggplot(aes(posixct, hr_est_95, color = animal != 'T_169')) +
    facet_wrap(~ group) +
    geom_line(aes(group = animal)) +
    geom_point() +
    labs(x = NULL, y = expression(bold('Home-range'~size~(km^2)))) +
    scale_color_manual(NULL, values = c('red3', '#00000016'),
                       labels = c('Deer T_169', 'Other deer')) +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.92, 0.8)),
  mw %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group)) %>%
    ggplot(aes(posixct, diffusion_km2_day, color = animal != 'T_169')) +
    facet_wrap(~ group) +
    geom_line(aes(group = animal)) +
    geom_point() +
    labs(x = NULL, y = expression(bold(Diffusion~(km^2/day)))) +
    scale_color_manual(values = c('red3', '#00000016')) +
    theme(legend.position = 'none'),
  d %>%
    mutate(group = if_else(group == 'Ovariectomy', 'Treatment', group)) %>%
    ggplot(aes(date, excursivity, group = animal,
                color = animal != 'T_169')) +
    facet_wrap(~ group) +
    geom_point() +
    geom_line() +
    labs(x = NULL, y = 'Excursivity') +
    scale_color_manual(values = c('red3', '#00000016')) +
    theme(legend.position = 'none'),
  ncol = 1)
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
bm <- readRDS('data/cleaned-telemetry-data.rds') %>%
  unnest(tel) %>%
  filter(! outlier) %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs('EPSG:4326') %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(2000) %>%
  st_bbox() %>%
  `names<-`(c('left', 'bottom', 'right', 'top')) %>%
  get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 14)

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
  labels = 'AUTO')

ggsave('figures/deer-T_169-comparison-ud.png', width = 10, height = 8,
       bg = 'white')

# drop T_169 ----
mw <- filter(mw, animal != 'T_169')
d <- filter(d, animal != 'T_169')

# home range size ----
m_hr <- bam(
  hr_est_95 ~
    group + #' `by` requires an explicit intercept for each group
    s(doy, by = group, k = 10, bs = 'cc') +
    s(doy_cr, by = group, animal_year, k = 10, bs = 'fs',
      xt = list(bs = 'cr')),
  family = Gamma('log'),
  data = mw,
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)))

appraise(m_hr, point_alpha = 0.1)
draw(m_hr, nrow = 3, parametric = TRUE, residuals = TRUE)
saveRDS(m_hr, file = 'models/m-hr-without-T_169.rds')

# diffusion ----
m_diff <- bam(
  diffusion_km2_day ~
    group + #' `by` requires an explicit intercept for each group
    s(doy, by = group, k = 10, bs = 'cc') +
    s(doy_cr, by = group, animal_year, k = 10, bs = 'fs',
      xt = list(bs = 'cr')),
  family = Gamma('log'),
  data = mw,
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)))

appraise(m_diff, point_alpha = 0.1)
draw(m_diff, nrow = 3, parametric = TRUE, residuals = TRUE)
saveRDS(m_diff, file = 'models/m-diff-without-T_169.rds')

# excursivity (fits in ~ 80 seconds) ----
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

ggplot(d, aes(fitted(m_exc), excursivity)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'darkorange') +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 20))
