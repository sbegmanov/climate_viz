library(tidyverse)

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

# zones used for
bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", "24S-EQU", 
           "44S-24S", "64S-44S", "90S-64S"))

zone_data <- read_csv(url) %>% 
  select(year = Year, all_of(bands)) %>% 
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>% 
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone))

current_year <- zone_data %>% 
  filter(year == 2022)

zone_data %>% 
  ggplot(aes(x = t_diff, xend = t_diff, 
             y = zone_position - 0.25, yend = zone_position + 0.25)) +
  geom_segment(color = "white", alpha = 0.25) +
  geom_segment(data = current_year,
               aes(color = t_diff),
               size = 2,
               lineend = "round") +
  scale_y_continuous(breaks = 1:8,
                     labels = bands) +
  scale_x_continuous(breaks = seq(-3, 4, 1),
                     labels = seq(-3, 4, 1),
                     limits = c(-3, 4)) +
  scale_color_gradient2(low = "darkblue", mid = "white", high = "darkred", 
                        midpoint = 0,
                        guide = "none") +
  labs(
    x = "Temperature anomaly (\u00B0C)",
    y = NULL,
    title = "Variation in annual temperature anomaly \n by latitude(1880-2022)",
    subtitle = "Bars for 2022 are colored by the size of the anomaly"
  ) +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "gray", size = 8),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid.major.x = element_line(color = "gray", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("figures/latitude_anomaly.png", width = 4, height = 3, units = "in")
