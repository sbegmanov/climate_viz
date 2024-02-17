source("code/local_weather.R")

library(magrittr)
library(bench)

no_na_no_zero <- local_weather[!is.na(local_weather$prcp) & 
                                 !is.na(local_weather$snwd) &
                                 local_weather$snwd > 0, ]

cor.test(no_na_no_zero$prcp, no_na_no_zero$snwd)
cor.test(~prcp + snwd, data = no_na_no_zero)

no_nas <- drop_na(local_weather)
no_nas_no_zero <- filter(no_nas, no_nas$snwd > 0)
cor.test(~prcp + snwd, data = no_nas_no_zero)

no_nas_no_zero <- local_weather |>
  drop_na() |>
  filter(snwd > 0)
  
cor.test(~prcp + snwd, data = no_nas_no_zero)

local_weather %<>% 
  drop_na() %>% 
  filter(snwd > 0)

local_weather %>% 
  drop_na() %>% 
  filter(snwd > 0) %>% 
  cor.test(~prcp + snwd, data = .)

local_weather %>% 
  drop_na() %>% 
  filter(snwd > 0) %$%  # exposition pipe
  cor.test(~prcp + snwd, data = .)

local_weather %>% 
  drop_na() %>% 
  filter(snwd > 0) %T>%
  plot %>% 
  summarize(total_prcp = sum(prcp))

local_weather %>% 
  drop_na() %T>% 
  print() %>% 
  filter(snwd > 0) %T>%
  print() %>% 
  summarize(total_prcp = sum(prcp))

plate <- 1:96 %>% 
  matrix(ncol = 12) %>% 
  set_colnames(LETTERS[1:12]) %>% 
  set_rownames(1:8) %>% 
  add(10) %>% 
  divide_by(10) %>% 
  as.data.frame() %>% 
  use_series(D)

plate <- 1:96 %>% 
  matrix(ncol = 12) %>% 
  set_colnames(LETTERS[1:12]) %>% 
  set_rownames(1:8) %>% 
  add(10) %>% 
  divide_by(10) %>% 
  as.data.frame() %>% 
  extract("D")

plate <- 1:96 %>% 
  matrix(ncol = 12) %>% 
  set_colnames(LETTERS[1:12]) %>% 
  set_rownames(1:8) %>% 
  add(10) %>% 
  divide_by(10) %>% 
  as.data.frame() %>% 
  extract2


bench_marked_pipe <- bench::mark(

base_nopipe = cor.test(~prcp + snwd, data = local_weather[!is.na(local_weather$prcp) & 
                                                          !is.na(local_weather$snwd) &
                                                          local_weather$snwd > 0, ]),

base_nopipe_s = cor.test(~prcp + snwd, data = subset(local_weather, !is.na(prcp) & 
                                                            !is.na(snwd) &
                                                            snwd > 0)),
dplyr_base = local_weather |>
  drop_na() |> 
  filter(snwd > 0) |> 
  cor.test(~prcp + snwd, data = _),

base_magrittr = local_weather |>
  subset(!is.na(prcp) & !is.na(snwd)) |> 
  subset(snwd > 0) |> 
  cor.test(~prcp + snwd, data = _),

base_magrittr_s = local_weather %>% 
  subset(!is.na(prcp) & !is.na(snwd) & snwd > 0) %>% 
  cor.test(~prcp + snwd, data = .),

dplyr_magrittr = local_weather %>% 
  drop_na() %>% 
  filter(snwd > 0) %>% 
  cor.test(~prcp + snwd, data = .)
)

autoplot(bench_marked_pipe, type = "jitter")





















