library(pacman)
p_load(tidyverse, janitor, lubridate, ggbeeswarm)

raw_data <- read_csv("result.csv")

df <- raw_data |>
  clean_names() |> 
  mutate(connection_since = as.Date(connection_since, "%d/%m/%y"),
         month = month(connection_since)) |> 
  as.data.frame()

df |> glimpse()


