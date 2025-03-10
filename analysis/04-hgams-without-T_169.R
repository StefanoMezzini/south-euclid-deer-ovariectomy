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

# moving window data (dropping T_169 later) ----
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
  subset = hr_est_95 < 10,
  method = 'fREML',
  discrete = TRUE,
  knots = list(doy = c(0.5, 365.5)))

appraise(m_hr, point_alpha = 0.1)
draw(m_hr, nrow = 3, parametric = TRUE, residuals = TRUE)
saveRDS(m_hr, file = 'models/m-hr-without-T_169.rds')

# diffusion ----
filter(mw, diffusion_km2_day > 2) # no values > 2

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

# plot predicted vs observed
ggplot(d, aes(fitted(m_exc), excursivity)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'darkorange') +
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 20))
