library('ctmm')   # for movement models
library('dplyr')  # for data wrangling
library('tidyr')  # for data wrangling
library('purrr')  # for functional programming
library('sf')     # for spatial data
library('terra')  # to work with rasters
library('ggplot2') # for fancy figures
source('analysis/figures/default-ggplot-theme.R')

tel <- readRDS('models/full-telemetry-movement-models.rds') %>%
  filter(animal == 'C_100') %>%
  pull(tel) %>%
  first() %>%
  data.frame() %>%
  mutate(date = as.Date(timestamp)) %>%
  select(x, y, date)

a <- readRDS('models/full-telemetry-movement-models.rds') %>%
  filter(animal == 'C_100') %>%
  pull(ud) %>%
  first() %>%
  raster(DF = 'CDF') %>%
  as.data.frame(xy = TRUE)

ggplot() +
  coord_equal() +
  geom_raster(aes(x / 1e3, y / 1e3, fill = layer), a) +
  geom_point(aes(x / 1e3, y / 1e3), tel, pch = '.', alpha = 0.1) +
  geom_path(aes(x / 1e3, y / 1e3), tel, alpha = 0.1, linewidth = 0.2) +
  geom_contour(aes(x / 1e3, y / 1e3, z = layer), a, color = 'black',
               breaks = seq(0, 1, by = 0.1), linewidth = 0.2) +
  scale_x_continuous('x (km)', expand = c(0, 0)) +
  scale_y_continuous('y (km)', expand = c(0, 0)) +
  scale_fill_gradient('AKDE quantile', low = '#327556', high = 'white',
                      limits = c(0, 1), aesthetics = c('color', 'fill')) +
  theme(legend.position = 'top')

ggsave('figures/ud-density-example.png', width = 7, height = 5,
       units = 'in', dpi = 600, bg = 'white')
