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

# South Euclid boundaries
# from https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-ohio-current-place-state-based
se <- read_sf('data/ohio-shp/tl_2019_39_place.shp') %>%
  filter(NAME == 'South Euclid') %>%
  st_geometry() %>%
  st_as_sf() %>%
  st_transform('EPSG:4326')

# S. Green Road 
green_rd <-
  tibble(
    long = -81 - c(.546, .542, .526, .521, .520, .5195, .5195, .517, .517),
    lat = 41 + c(.554, .550, .538, .534, .531, .520, .501, .498, .486)) %>%
  st_as_sf(coords = c('long', 'lat')) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_set_crs('EPSG:4326')

# background map of telemetry locations and stations, buffered by 1 km
if(file.exists('data/south-euclid-basemap.rds')) {
  se_map <- readRDS('data/south-euclid-basemap.rds')
} else {
  se_map <- bind_rows(stations, tels) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_as_sf() %>%
    st_buffer(1e3) %>%
    st_bbox() %>%
    `names<-`(c('left', 'bottom', 'right', 'top')) %>%
    get_stadiamap(maptype = 'stamen_terrain', bbox = ., zoom = 14)
  saveRDS(se_map, 'data/south-euclid-basemap.rds')
}

p_a <-
  ggmap(se_map) +
  geom_path(aes(X, Y, group = animal), bind_cols(tels, st_coordinates(tels)),
            inherit.aes = FALSE, alpha = 0.5, linewidth = 0.1) +
  geom_sf(data = tels, inherit.aes = FALSE, alpha = 0.1, size = 0.2) +
  geom_sf(inherit.aes = FALSE, data = se, color = 'darkred', lwd = 0.5,
          fill = 'transparent') +
  geom_sf(data = green_rd, inherit.aes = FALSE, color = 'darkorange',
          linewidth = 1) +
  geom_sf(data = stations, inherit.aes = FALSE, size = 3.5) +
  geom_sf(data = stations, inherit.aes = FALSE, size = 1.75, color = 'white') +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_x_continuous(breaks = seq(-81.56, -81.48, by = 0.02),
                     expand = c(0, 0)) +
  geom_rect(aes(xmin = -81.56, xmax = -81.524,
                ymin = 41.446, ymax = 41.4524),
            fill = 'white', color = 'black', linewidth = 0.5) +
  ggspatial::annotation_scale(style = 'ticks', text_cex = 0.9,
                              line_width = 1)

se_center <- st_as_sf(st_centroid(se))

us_states <- st_transform(us_states, 'EPSG:32617')

ohio <- filter(us_states, NAME == 'Ohio')

p_b <- ggplot() +
  geom_sf(data = ohio, fill = 'black') +
  geom_sf(data = se_center, color = 'darkorange', size = 2) +
  theme_map()

p_c <- ggplot() +
  geom_sf(data = us_states, fill = 'black') +
  geom_sf(data = ohio, fill = 'darkorange') +
  theme_map()

p_map <-
  plot_grid(p_a,
            plot_grid(p_b, p_c, labels = c('b', 'c'), nrow = 2),
            labels = c('a', ''), ncol = 2, rel_heights = c(6, 1))

ggsave('figures/south-euclid-map.png', p_map, scale = 2,
       width = WIDTH, height = WIDTH, units = 'in', dpi = 600, bg = 'white')

# find mean number of locations inside and outside of South Euclid ----
# without grouping by animal
tels %>%
  mutate(.,
         group = if_else(grepl('C', animal), 'Control', 'Treatment'),
         inside = st_intersects(., se, sparse = FALSE)[, 1]) %>%
  st_drop_geometry() %>%
  group_by(group) %>%
  summarize(prop_inside_se = mean(inside))

# without grouping by animal and without 169
tels %>%
  filter(animal != 'T_169') %>%
  mutate(.,
         group = if_else(grepl('C', animal), 'Control', 'Treatment'),
         inside = st_intersects(., se, sparse = FALSE)[, 1]) %>%
  st_drop_geometry() %>%
  group_by(group) %>%
  summarize(prop_inside_se = mean(inside))

# grouping by animal: proportion of fixes
p_inside_a <- tels %>%
  mutate(.,
         group = if_else(grepl('C', animal), 'Control', 'Treatment'),
         inside = st_intersects(., se, sparse = FALSE)[, 1]) %>%
  st_drop_geometry() %>%
  group_by(group, animal) %>%
  summarize(prop_inside_se = mean(inside)) # by animal

# with T_169
p_inside_a %>% # still grouped by treatment group
  summarize(prop_inside_se = mean(prop_inside_se)) # by group

# without T_169
p_inside_a %>% # still grouped by treatment group
  filter(animal != 'T_169') %>%
  summarize(prop_inside_se = mean(prop_inside_se)) # by group
