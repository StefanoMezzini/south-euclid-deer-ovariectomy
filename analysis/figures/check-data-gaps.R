library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for funtional programming
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('khroma')  # for rainbow color palette
source('analysis/figures/default-ggplot-theme.R')

# gaps do not appear to depend on the deer's location ----
d <- readRDS('data/cleaned-telemetry-data.rds') %>%
  select(! tag_local_identifier) %>%
  unnest(tel) %>%
  filter(outlier == 0) %>%
  group_by(animal) %>% # to set time intervals correctly
  mutate(interval_h = c(NA, as.numeric(diff(timestamp), units = 'hours')),
         doy = lubridate::yday(timestamp)) %>%
  ungroup() %>%
  filter(! is.na(interval_h))

d %>%
  group_by(group) %>%
  summarize(median = median(interval_h),
            IQR = IQR(interval_h),
            mean = mean(interval_h),
            sd = sd(interval_h))

ggplot(d, aes(long, lat, color = lubridate::decimal_date(timestamp))) +
  facet_wrap(~ animal, scales = 'free') +
  geom_path(alpha = 0.2) +
  geom_point(alpha = 0.6) +
  labs(x = NULL, y = NULL) +
  scale_color_smoothrainbow(name = 'Date', limits = c(2023, 2024.5),
                            breaks = c(2023, 2023.5, 2024, 2024.5)) +
  theme(legend.key.width = rel(4), legend.position = 'inside',
        legend.position.inside = c(2/5 + 3/5/2, 0.125),
        legend.direction = 'horizontal')

ggsave('figures/telemetries.png', width = 16, height = 16, dpi = 300,
       bg = 'white')

# T_169 moves to the secondary location 7 separate times ----
d %>%
  filter(animal == 'T_169') %>%
  filter(lat < 41.5) %>%
  pull(timestamp) %>%
  as.Date() %>%
  hist(breaks = 'days')

d %>%
  filter(animal == 'T_169') %>%
  filter(lat < 41.5) %>%
  pull(timestamp) %>%
  quantile(probs = seq(0, 1, by = 0.05)) %>%
  as.Date()

dates <- as.Date(c('2023-02-01', '2023-02-08', '2023-04-08', '2023-04-15',
                   '2023-04-30', '2023-05-11', '2023-05-18', '2023-05-30'))
abline(v = dates, col = 'red3')

d %>%
  filter(animal == 'T_169') %>%
  mutate(excursion = lat < 41.5,
         trip = case_when(timestamp < dates[1] ~ 1,
                          timestamp < dates[2] ~ 2,
                          timestamp < dates[3] ~ 3,
                          timestamp < dates[4] ~ 4,
                          timestamp < dates[5] ~ 5,
                          timestamp < dates[6] ~ 6,
                          timestamp < dates[7] ~ 7,
                          timestamp < dates[8] ~ 8)) %>%
  group_by(trip) %>%
  mutate(n = sum(excursion),
         days = diff(range(timestamp[which(excursion)])) / 60^2 / 24,
         days = round(days),
         start = as.Date(min(timestamp[which(excursion)])) %>%
           format(format = '%B %d'),
         end = as.Date(max(timestamp[which(excursion)])) %>%
           format(format = '%B %d')) %>%
  ungroup() %>%
  mutate(lab = paste0('Trip ', trip, ' (', n, ' fixes in ', days,
                      if_else(days == 1, ' day)', ' days)'),
                      ':\nfrom ', start, ' to ', end) %>%
           gsub(' 0', ' ', .)) %>%
  ggplot(aes(long, lat)) +
  coord_equal() +
  facet_wrap(~ lab, nrow = 2) +
  geom_path() +
  geom_path(aes(color = excursion)) +
  geom_point(aes(color = excursion), alpha = 0.3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_x_continuous(breaks = seq(-81.54, -81.51, by = 0.01)) +
  scale_color_manual('Excursion', values = c('black', 'red')) +
  theme(legend.position = 'top')

ggsave('figures/T_169-excursions.png', width = 10, height = 10, dpi = 600,
       bg = 'white')

# density plots of data gaps look similar between groups ----
mw <- map_dfr(list.files('models/moving-windows',
                         pattern = '-window-7-days-dt-3-days.rds',
                         full.names = TRUE), readRDS) %>%
  select(! c(hr_lwr_95, hr_upr_95)) %>%
  mutate(doy = lubridate::yday(date),
         group = factor(stringr::str_to_sentence(group)),
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

# no clear trends in control group has more deer with hourly fixes
mw %>%
  mutate(n = map_int(dataset, nrow)) %>%
  ggplot(aes(n, group = animal, color = group)) +
  facet_grid(group ~ .) +
  geom_vline(xintercept = 24 * 7, color = 'grey') +
  geom_density(linewidth = 1, adjust = 0.5, alpha = 0.3) +
  labs(x = 'Number of fixes in a window', y = 'Density') +
  scale_color_manual('Group', values = PAL) +
  theme(legend.position = 'none')
