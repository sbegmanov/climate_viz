library(tidyverse)

tibble(theta = 2 * pi * seq(0, 10, 0.05), 
       radius = seq(0, 1, length.out = length(theta))) %>% 
  mutate(x = radius * sin(theta),
         y = radius * cos(theta)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path() 
