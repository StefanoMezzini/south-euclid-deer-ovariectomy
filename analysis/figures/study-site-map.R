library('dplyr')   # for data wrangling
library('sf')      # for spatial data
library('spData')  # for US shapefile
library('ggplot2') # for fancy figures
library('ggmap')   # for basemaps
library('cowplot') # for fancy multi-panel figures
source('analysis/figures/default-ggplot-theme.R')

# deer telemetry data
tels <-
  readRDS('data/cleaned-telemetry-data.rds') %>%
  ungroup() %>%
  tidyr::unnest(tel) %>%
  filter(! outlier) %>%
  select(animal, long, lat) %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs('EPSG:4326')

# coordinates of location stations
stations <- tibble(
  name = c('Euclid Creek Reservation',
           'Shaker Heights Fire Dept. Station #2',
           'South Euclid City Hall',
           'South Euclid-Lyndhurst Branch Library'),
  long = c(-81.531, -81.536, -81.518, -81.520),
  lat = c(41.561, 41.479, 41.523, 41.509)) %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs('EPSG:4326')

green_rd <-
  tibble(
    long = -81 - c(.546, .542, .526, .521, .520, .5195, .5195, .517, .517),
    lat = 41 + c(.554, .550, .538, .534, .531, .520, .501, .498, .486)) %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_set_crs('EPSG:4326')

# project to UTM 17N
# stations_utm <- st_transform(stations, 'EPSG:26917')

area <- bind_rows(stations, tels) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(1e3)

se_map <- area %>%
  st_bbox() %>%
  `names<-`(c('left', 'bottom', 'right', 'top')) %>%
  get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 14)

p_a <-
  ggmap(se_map) +
  geom_path(aes(X, Y, group = animal), bind_cols(tels, st_coordinates(tels)),
            inherit.aes = FALSE, alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = tels, inherit.aes = FALSE, alpha = 0.5, size = 0.2) +
  geom_sf(data = green_rd, inherit.aes = FALSE, color = 'darkorange',
          linewidth = 2) +
  geom_sf(data = stations, inherit.aes = FALSE, size = 3.5) +
  geom_sf(data = stations, inherit.aes = FALSE, size = 1.75, color = 'white') +
  labs(x = 'Longitude', y = 'Latitude')

se <- stations %>%
  select(! name) %>%
  st_union() %>%
  st_centroid() %>%
  st_as_sf()

us_states <- st_transform(us_states, 'EPSG:32617')

ohio <- filter(us_states, NAME == 'Ohio')

p_b <- ggplot() +
  geom_sf(data = ohio, fill = 'black') +
  geom_sf(data = se, color = 'darkorange', size = 2) +
  theme_map()

p_c <- ggplot() +
  geom_sf(data = us_states, fill = 'black') +
  geom_sf(data = ohio, fill = 'darkorange') +
  theme_map()

plot_grid(p_a,
          plot_grid(p_b, p_c, labels = c('B', 'C'), nrow = 1),
          ncol = 1, rel_heights = c(6, 1))

ggsave('figures/south-euclid-map.png',
       width = 7, height = 14 * (7/6), units = 'in', dpi = 600, bg = 'white')
