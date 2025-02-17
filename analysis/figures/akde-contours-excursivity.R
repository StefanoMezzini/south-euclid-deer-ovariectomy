library('ctmm')    # for movement models
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('sf')      # for spatial data
library('terra')   # to work with rasters
library('ggplot2') # for fancy figures
library('ggmap')   # for basemaps
source('analysis/figures/default-ggplot-theme.R')

tel <- readRDS('models/full-telemetry-movement-models.rds') %>%
  filter(animal == 'C_100') %>%
  pull(tel) %>%
  first() %>%
  data.frame() %>%
  transmute(x_start = longitude,
            x_end = lag(longitude),
            y_start = latitude,
            y_end = lag(latitude),
            timestamp = as.POSIXct(timestamp))

a <- readRDS('models/full-telemetry-movement-models.rds') %>%
  filter(animal == 'C_100') %>%
  pull(ud) %>%
  first() %>%
  raster(DF = 'CDF') %>%
  rast() %>%
  project('EPSG:4326') %>%
  as.data.frame(xy = TRUE) %>%
  filter(layer < 0.99)

# basemap
bm <- tel %>%
  st_as_sf(coords = c('x_start', 'y_start')) %>%
  st_set_crs('EPSG:4326') %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(750) %>%
  st_bbox() %>%
  `names<-`(c('left', 'bottom', 'right', 'top')) %>%
  get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 15)

ggmap(bm) +
  coord_equal() +
  geom_segment(aes(x = x_start, xend = x_end, y = y_start, yend = y_end,
                   group = factor(timestamp)), tel, size = 0.1, alpha = 0.5) +
  geom_point(aes(x = x_start, y = y_start), tel, size = 0.1, alpha = 0.5) +
  geom_raster(aes(x, y, fill = layer), a, na.rm = TRUE) +
  geom_contour(aes(x, y, z = layer), a, color = 'white',
               breaks = seq(0, 1, by = 0.1), linewidth = 0.1,
               na.rm = TRUE, inherit.aes = FALSE) +
  scale_x_continuous('Longitude', expand = c(0.02, 0),
                     limits = range(tel$x_start)) +
  scale_y_continuous('Latitude', expand = c(0.02, 0),
                     limits = range(tel$y_start)) +
  scale_fill_gradient('AKDE quantile', low = 'darkorange3',
                      high = '#cd660002', limits = c(0, 1),
                      aesthetics = c('color', 'fill')) +
  theme(legend.position = 'top')

ggsave('figures/ud-density-example.png', width = 5, height = 5,
       units = 'in', dpi = 600, bg = 'white')
