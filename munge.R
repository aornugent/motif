library(tsmp)
library(tidyverse)
library(snakecase)
library(lubridate)

# Second data is missing, but frequency seems to be 0.5Hz
x <- read_csv("data/POVI 56 44780.csv", skip = 4) %>%
  rename_all(to_any_case) %>%
  filter(!is.na(gmt_time)) %>%
  mutate(across(gmt_time, parse_datetime, format = "%d/%m/%Y %H:%M"),
         across(gmt_time, ~ . + seconds(seq(0, 58, len = 30)))) 

# this takes a while to draw, but show's it's contiguous data
# Interesting that accelerometers don't zero out when they're stationary
select(x, -temperature_c) %>%
  gather(dimension, value, -gmt_time) %>%
  ggplot(., aes(x = gmt_time, y = value, colour = dimension)) +
  geom_line() + 
  theme_bw()

# Dunno if it's important but we can difference into rate of change.
dx <- mutate(x, across(x:z, ~c(0, diff(.))))

select(dx, -temperature_c) %>%
  gather(dimension, value, -gmt_time) %>%
  ggplot(., aes(x = gmt_time, y = value, colour = dimension)) +
  geom_line() + 
  theme_bw()          


mp <- as.matrix(dx[1:100000, 2:4]) %>%
  tsmp(., window_size = 1800,
       mode = "mstomp",
       n_workers = 1)

f <- find_motif(mp, n_motif = 10,
                n_neighbor = 2,
                n_bit = 16,
                n_dim = 3,
                mode = "unconstrained")

plot(f)
