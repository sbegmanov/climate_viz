library(tidyverse)
library(glue)

# unventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

unventory_url <- "C:\\Users\\salam\\Desktop\\climate_viz\\ghcnd-inventory.txt"

inventory <- read_table(unventory_url, 
                        col_names = c("station", "lat", "lon", "variable", "start", "end"))

my_lat <- 42.33831964441621 * 2 * pi / 360
my_lon <- 83.88938389977316 * 2 * pi / 360

# my_lat <- 42.433650868471304 * 2 * pi / 360
# my_lon <- 59.574049086442116 * 2 * pi / 360




my_station <- inventory %>% 
  mutate(lat_r = lat * 2 * pi / 360,
         lon_r = lon * 2 * pi / 360,
         d = 1.609344 * 3963 * acos((sin(lat_r) * sin(my_lat)) + cos(lat_r) * cos(my_lat) * cos(my_lon - lon_r))
         ) %>% 
  filter(start < 1960 & end > 2020) %>% 
  top_n(n = -1, d) %>% 
  distinct(station) %>% 
  pull(station)

# station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

station_daily <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USC00200230.csv.gz"
local_weather <- read_csv(station_daily,
                          col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>% 
  select(date, variable, value) %>% 
  pivot_wider(names_from = "variable", 
              values_from = "value") %>% 
  select(date, TMAX, PRCP, SNWD) %>% 
  mutate(date = ymd(date),
         TMAX = TMAX / 10,
         PRCP = PRCP / 10) %>% 
  rename_all(tolower) %>% 
  mutate(snwd = if_else(snwd < 500, snwd, NA_real_),
         prcp = if_else(prcp < 200, prcp, NA_real_))





