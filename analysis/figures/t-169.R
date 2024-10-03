library('readr')   # for importing csv files in a tidy way
library('dplyr')   # for data wrangling
library('sf')      # for spatial data
library('ggplot2') # for fancy figures
library('ggmap')   # for basemaps
library('ctmm')    # for movement modeling
source('analysis/figures/default-ggplot-theme.R')

# telemetry data
d <- read_csv('data/Odocoileus virginianus DeNicola South Euclid.csv',
              show_col_types = FALSE) %>%
  filter(`individual-local-identifier` == 'T_169') %>%
  rename_with(\(.names) gsub('-', '_', .names)) %>%
  rename_with(\(.names) gsub('location_', '', .names))

# basemap
bm <- d %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs('EPSG:4326') %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(100) %>%
  st_bbox() %>%
  `names<-`(c('left', 'bottom', 'right', 'top')) %>%
  get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 12)

ggmap(bm) +
  geom_path(aes(long, lat), d)
