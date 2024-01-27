source("code/local_weather.R")

this_year <- year(today()) - 1

local_weather %>% 
  select(date, tmax) %>% 
  drop_na(tmax) %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1891 & year != this_year) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax)) %>% 
  mutate(normalized_range = (year >= 1951 & year <= 1980),
         normalized_mean = sum(tmax * normalized_range) / sum(normalized_range),
         t_diff = tmax - normalized_mean) %>% 
  ggplot(aes(x = year, y = t_diff)) +
  geom_line() +
  geom_smooth()


local_weather %>% 
  select(date, tmax) %>% 
  drop_na(tmax) %>% 
  mutate(year = year(date),
         month = month(date)) %>% #filter(year == 1951 & month == 10) %>% print(n = Inf)
  # filter(year != 1891) %>% 
  group_by(year, month) %>% 
  summarize(tmax = mean(tmax), .groups = "drop") %>% 
  group_by(month) %>% 
  mutate(normalized_range = year >= 1951, year <= 1980,
         normalized_temp = sum(tmax * normalized_range) / sum(normalized_range),
         t_diff = tmax - normalized_temp,
         is_this_year = year == this_year) %>% 
  ungroup() %>% 
  # filter(month == 10) %>% slice_min(t_diff, n = 5) %>% 
  ggplot(aes(x = month, y = t_diff, group = year, color = is_this_year)) +
  geom_line() +
  scale_color_manual(breaks = c(F, T),
                     values = c("lightgray", "dodgerblue"),
                     guide = "none") +
  theme_classic()

  
  
  
  
  